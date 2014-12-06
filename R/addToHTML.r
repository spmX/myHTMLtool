#' @title Add data frame to HTML file
#'
#' @description  Add table data stored as R data.frame format to a report file in HTML format
#'
#' @param dataframe data.frame R data frame containg table data to be added to the report
#' @param jsonFilePath Path to .json file containing table data
#' @param htmlObj  S3 object created by initHTML() 
#' @param customFilter vector  User-defined column filtering criteria. See Details.
#' @param tableID character id of the data table to be added to html file
#' @param fieldnames vector Field names of data frame
#' @param figurePath Path to the figure
#' @param figureID character id of the figure to be added to html file
#' 
#' @return .html file Report file containing data table 
#'
#' @details When customFilter is set to "auto",it automatically create individual column filters
#'  based on data type of each col. For instance, if a column contains characters, it's column filter 
#'  will be a search box ; if a column contains  numbers,it's column filter will be range filter;
#'  User can also specify which specific column(s) need to be filter.
#'  For example, if user wants to set the 4th column a five-column table to be filtered by keyword search, 
#'  simply set:
#'      customFilter = c("null","null","null","search","null")
#'  The output of this function is a .html file, along with required css and javascript library, as well as 
#'  a data folder containing the .json file. 
#' 
#' @return htmlObj The updated htmlObj object containing the data table and javascripts required for parsing the table
#'
#' @examples
#' outDir = "."  
#' data(gwas)
#' data(DEgene)
#' htmlObj = initHTML(title="Report Title",outDir=outDir,outPrefix="Example",author="Author Name")  	
#' htmlObj = addLogo(htmlObj)
#
#' # use report template
#' template = system.file("extdata/rmarkdown/report.template.rmd",package="myHTMLtool")
#' htmlObj = addReport(template,htmlObj)
#' # add data to the report, noted that tableID should be the same as table id specified in report template
#' htmlObj = addDataFrameToHTML(dataframe=gwas.df,htmlObj=htmlObj,customFilter="auto",tableID="mytable1")
#' htmlObj = addDataFrameToHTML(dataframe=DEgene.df,htmlObj=htmlObj,customFilter="auto",tableID="mytable2")
#'
#' @rdname addToHTML
#'
#' @export
addDataFrameToHTML <- function(dataframe,htmlObj,customFilter="auto",tableID=NULL,fieldnames=NULL)
{
 	#=====================================
	# updated on Nov-10-2014
	# for reason I haven't figured out, the fieldnames should not contain period ".",
	# otherwise, .json file could not be loaded correctly
	# Thus the following function is created to convert "." into "_"
	process.col.names <- function(data.frame) {
		names = colnames(data.frame)
		
		for (i in 1:length(names)) {
			name.before = names[i]
			names[i] = gsub("\\.","\\_",name.before)
		}
		return(names)
	}
	
    if(is.null(fieldnames)) {
		new.colnames = process.col.names(dataframe)
		colnames(dataframe) = new.colnames
	}
	# end of update
	#===================================   
	fieldnames = colnames(dataframe)

	# create datatableNode, fieldnames should be exactly the same as
	# keys in json file
	rootNode = xmlRoot(htmlObj$htmlDoc)
	headNode = rootNode[["head"]]
	bodyNode = rootNode[["body"]]

	#===============================
	# updated nov-19-2014
	if (!is.null(tableID))  {
		path = sprintf("//div[@id='%s']",tableID)
		tableDest =  xpathApply(bodyNode,path)
		if (length(tableDest ) > 0) {
			tableParentNode = tableDest[[1]]
			removeAttributes(tableParentNode,attrs="id")
		} else tableParentNode = bodyNode
	} 
	else {
		tableID="myHTMLtoolTable"
		tableParentNode = bodyNode }	
	# end of update
	#===============================
	
	# convert data frame into a json file
    jsonDir = htmlObj$dataDir
    jsonFilePath = convertToJson(dataframe,outDir=jsonDir,outPrefix=tableID)
    
    jsonFileName = basename(jsonFilePath)
    jsonFileRelativePath = file.path("./Rdata",jsonFileName)

	# check if jsonFilePath is valid
	#if(!file.exists(jsonFilePath)) stop("json file does not exist")
	#if(length(dir(htmlObj$dataDir)) == 0) file.copy(jsonFilePath,htmlObj$dataDir)
	
	dataTableNodes = createTableNodes(fieldnames,tableID)
	wrapNode = XML::newXMLNode("div",attrs= c("class"="table_container"),
							  .children=c(dataTableNodes),parent=tableParentNode)
	
	# Now create custom javascript to handle json file
	scriptNode = createScriptNode(dataframe,fieldnames,customFilter,tableID,jsonFileRelativePath,headNode)  
	
	return(htmlObj)
}

#' @rdname addToHTML
#'
#' @export
addJsonFileToHTML <- function(jsonFilePath,htmlObj,fieldnames,customFilter=NULL,tableID=NULL)
{
	# check if jsonFilePath is valid
	if(!file.exists(jsonFilePath)) stop("json file does not exist")
	
	file.copy(jsonFilePath,htmlObj$dataDir)

    # add relative path, so that .html file can directly read .json from .Rdata/
    jsonFileName = basename(jsonFilePath)
    jsonFileRelativePath = file.path("./Rdata",jsonFileName)

	# create datatableNode, fieldnames should be exactly the same as
	# keys in json file
	rootNode = xmlRoot(htmlObj$htmlDoc)
	headNode = rootNode[["head"]]
	bodyNode = rootNode[["body"]]
	
	#===============================
	# updated nov-19-2014
	if (!is.null(tableID))  {
		path = sprintf("//div[@id='%s']",tableID)
		tableDest =  xpathApply(bodyNode,path)
		if (length(tableDest ) > 0) {
			tableParentNode = tableDest[[1]]
			removeAttributes(tableParentNode,attrs="id")
		} else tableParentNode = bodyNode
	} 
	else {
		tableID="myHTMLtoolTable"
		tableParentNode = bodyNode }	
	# end of update
	#===============================
	
	dataTableNodes = createTableNodes(fieldnames,tableID)
	wrapNode = XML::newXMLNode("div",attrs= c("class"="table_container"),
							  .children=c(dataTableNodes),parent=tableParentNode )
	
	# Now create custom javascript to hander the json file
    dataframe = NULL
	scriptNode = createScriptNode(dataframe,fieldnames,customFilter,tableID,jsonFileRelativePath,headNode)  
	
	#publishHTML(htmlObj)
	return(htmlObj)
}

#' @rdname addToHTML
#'
#' @export
addFigureToHTML <- function(figurePath,htmlObj,figureID=NULL)
{
	# check if figurePath is valid
	if(!file.exists(figurePath)) stop("figure does not exist")
	
	file.copy(figurePath,htmlObj$dataDir)

    # add relative path, so that .html file can directly read .json from .Rdata/
    figureName = basename(figurePath)
    figureRelativePath = file.path("./Rdata",figureName)

	# create datatableNode, fieldnames should be exactly the same as
	# keys in json file
	rootNode = xmlRoot(htmlObj$htmlDoc)
	headNode = rootNode[["head"]]
	bodyNode = rootNode[["body"]]
	
	#===============================
	# updated nov-19-2014
	if (!is.null(figureID))  {
		path = sprintf("//div[@id='%s']",figureID)
		figureDest =  xpathApply(bodyNode,path)
		if (length(figureDest ) > 0) {
			figureParentNode = figureDest[[1]]
		} else figureParentNode = bodyNode
	} else {
		figureID = "myHTMLtoolFigure"
		figureParentNode = bodyNode 
	}	
	# end of updatae
	#===============================
	
	figureNode = createFigureNode(figureRelativePath)
	wrapNode = XML::newXMLNode("div",attrs= c("class"="img_container"),
							   .children=c(figureNode),parent=figureParentNode)
	
	#publishHTML(htmlObj)
	return(htmlObj)
}


#' @title Create .html report file
#'
#' @description Write the htmlObj to .html file
#'
#' @param htmlObj S3 object  created by initHTML()
#'
#' @export
publishHTML <- function(htmlObj)
{
	htmlDir = htmlObj$reportDir
	outPrefix = htmlObj$outPrefix
	htmlFilePath = file.path(htmlDir,sprintf("%s.html",outPrefix))
	htmlFile = file(htmlFilePath)
	
	htmlDoc = htmlObj$htmlDoc
	writeLines(saveXML(htmlDoc),htmlFile)
	message = sprintf("html file created and saved as %s.html,at %s",outPrefix,htmlDir)
	print(message)
	
	close(htmlFile)

}	


# title Create Table Node
createTableNodes <- function(fieldnames,tableID,tableClass="display") 
{
	# create nodes in a tree-structure, top-down
	header = fieldnames
	footer = fieldnames
	empty.header = rep("", length(header))
	
	# now create table, including thead and tfoot
	tableNode = newXMLNode("table",
							attrs=c("id"=tableID,
								"class"=tableClass,
								"style"="width:100%"))
	
	theadNode = newXMLNode("thead",parent=tableNode)
	# tfootNode = newXMLNode("tfoot",parent=tableNode)
	# invisible(lapply(footer,function(x){newXMLNode("th",x,parent=tfootNode)}))
	
	#noted that the <tr/> with empty header is used for 
	#adding indivisual search/filter function
	trNode1 = newXMLNode("tr",parent=theadNode)
	invisible(lapply(empty.header,function(x){newXMLNode("th",x,parent=trNode1)}))
	
	trNode2 = newXMLNode("tr",parent=theadNode)
	invisible(lapply(header,function(x){newXMLNode("th",x,parent=trNode2)}))
	
	tableNode = addChildren(tableNode,theadNode)
	# tableNode = addChildren(tableNode,tfootNode)
	
	tableNode
}

# create a cols variable 
createVarCols <- function(fieldnames)
{	
	out = "var cols = [ "
	for (i in 1:length(fieldnames)){
		out = paste(out,sprintf("{ \"data\": \"%s\" },",fieldnames[i]))
		
	}
	out = paste(out,"];\n")
	out
}

# Create individual column filters 
dfToCustomFilter <- function(dataframe)
{
    
	decoded = c()
    filter.class.map <- c(
			"integer" = "range",
			"numeric" = "range",
			"list" = "search",
			"character" = "search",
            "factor"  = "search")
			
    decoded = as.vector(filter.class.map[sapply(dataframe, class)])
	decoded[is.na(decoded)] <- "null"	

    decoded
}	
	

# create a java script  array to store col types
createVarColTypes <- function(dataframe,customFilter)
{
	if (is.null(customFilter)) {
        cf = c("null")
    }
	
	# customFilter: search, range, select, NULL
	else if (customFilter[1] == "auto"){
        cf = dfToCustomFilter(dataframe)
	}	
	else cf = customFilter

	# return a string in javascript array format:
	# colTypes = ["text",null,"num",null,null];
	varColTypes = "colTypes = ["
	for (i in 1:length(cf)) {
		if (cf[i] == "null") varColTypes = paste(varColTypes,cf[i],",") 
		else  varColTypes = paste(varColTypes,sprintf("\"%s\",",cf[i]) )
	}
	
	varColTypes = paste(varColTypes,"];\n")
	varColTypes
}

# create javascript node
createScriptNode <- function(dataframe=NULL,fieldnames,customFilter,tableID,jsonPath,parentNode)
{

	# store dom ready
	#$(document).ready(function(){
	ready = "\n$(document).ready(function(){\n"
	varCols = createVarCols(fieldnames)
	varColTypes = createVarColTypes(dataframe,customFilter)
	setTableId = sprintf("tableID = \"%s\";\n",tableID)
	setJsonPath = sprintf("jsonPath =  \"%s\";\n",jsonPath)
	loadJasonToTable = paste("loadJasonToTable(jsonPath,tableID,cols,colTypes,colDefs=[]);\n","});\n")

	#cat all script together
	customScript = paste(ready,varCols,varColTypes,setTableId,setJsonPath,loadJasonToTable)
	scriptNode = newXMLNode("script", newXMLTextNode(customScript), parent=parentNode)
	scriptNode

}


createFigureNode <- function(figurePath=NULL)
{
	aNode = newXMLNode("a",attrs=c(href=figurePath,target="_blank"))
	imgNode = newXMLNode("img",attrs=c(src=figurePath),parent=aNode)
	#figureNode = newXMLNode("div",attrs=c("style"="margin-left:auto;margin-right:auto"),.children=aNode)
	
	return(aNode)
}
