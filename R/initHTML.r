#' @import XML
NULL


#' @title Initiate an htmlObj
#' 
#' @description create new htmlObj, and set up folders containing required css and js libraries
#'
#' @param title character Report title
#' @param outDir path Path of output files
#' @param outPrefix character The string used for creating output folder 
#' @param author character Report author
#' @param date character Report date
#'
#' @return htmlObj
#'
#' @export
initHTML<- function(title="My Report",outDir=".",outPrefix="htmlReport",author="",date="")
{
	htmlObj = structure(list(),class="html")
	htmlObj[["outPrefix"]] = outPrefix
	
	# create sub_dir to store json data, css , javascript
    reportDir = file.path(outDir,outPrefix)
	print(sprintf("Initiating project at %s",reportDir))
	
	dataDir = file.path(reportDir,"Rdata")
	jsDir = file.path(reportDir,"Rjslib")
	cssDir = file.path(reportDir,"Rcsslib")
	
	if(!file.exists(dataDir)) dir.create(dataDir,recursive=TRUE)
	if(!file.exists(cssDir)) dir.create(cssDir,recursive=TRUE)
	if(!file.exists(jsDir)) dir.create(jsDir,recursive=TRUE)
	
	# copy css and js files from the pakcage
	css.files <- c(
				 system.file("extdata/csslib/DT_custom.css",package="myHTMLtool"),
				 system.file("extdata/csslib/logo.png",package="myHTMLtool")
				   )
	js.files <- c(
				 system.file("extdata/jslib/makeDataTable.0.1.js",
							 package="myHTMLtool"),
				 system.file("extdata/jslib/jquery.dataTables.columnFilter.modified.js",
							 package="myHTMLtool")
				  )
	file.copy(css.files,cssDir, overwrite=TRUE)
	file.copy(js.files,jsDir, overwrite=TRUE)
	
		
	# add variables to a html object
	htmlObj[["outDir"]] = outDir
    htmlObj[["reportDir"]] = reportDir
	htmlObj[["dataDir"]] = dataDir
	htmlObj[["cssDir"]] = cssDir
	htmlObj[["jsDir"]] = jsDir
	
	#===================================================
	# create htmlDoc 
	htmlObj[["htmlDoc"]] = XML::newHTMLDoc()
	
	csslib.files = c("//code.jquery.com/ui/1.11.1/themes/smoothness/jquery-ui.css",
				"//cdn.datatables.net/1.10.3/css/jquery.dataTables.css",
				"//cdn.datatables.net/tabletools/2.2.3/css/dataTables.tableTools.css",
				"./Rcsslib/DT_custom.css")
				
	jslib.files = c("//code.jquery.com/jquery-1.10.2.min.js",
					"//code.jquery.com/ui/1.11.1/jquery-ui.js",
					"//cdn.datatables.net/1.10.3/js/jquery.dataTables.js",
					"//cdn.datatables.net/tabletools/2.2.3/js/dataTables.tableTools.js",
					"//cdn.datatables.net/scroller/1.2.2/js/dataTables.scroller.js",
					"//cdn.datatables.net/plug-ins/a5734b29083/api/fnFilterClear.js",
					"./Rjslib/jquery.dataTables.columnFilter.modified.js",
					"./Rjslib/makeDataTable.0.1.js")

	
	#add meta data and title to headNode
	rootNode = xmlRoot(htmlObj$htmlDoc)
	headNode = rootNode[["head"]]
	bodyNode = rootNode[["body"]]
	metaNode = newXMLNode("meta",attrs=c("charset"="utf-8"),parent=headNode)
	invisible(newXMLNode("title",title, parent=headNode))
	
	# add title, author and date
	titleNode = newXMLNode("div",title, attrs=c(class="title"),parent=bodyNode,at=0)
	if(!(author == "")) {
		invisible(newXMLNode("br",parent=titleNode))
		invisible(newXMLNode("div",author, attrs=c(class="subtitle"), parent=titleNode))
	}
	
	if(date == "") date = as.character(Sys.Date())
	invisible(newXMLNode("div",date, attrs=c(class="subtitle"), parent=titleNode))
	

	# add css links and javascript links to the head
	invisible(lapply(csslib.files,
					function(csslink)
					{newXMLNode("link",
								attrs=c("rel"="stylesheet","type"="text/css","href"=csslink),
								parent=headNode)}))

	invisible(lapply(jslib.files,
					function(jslink)
					{newXMLNode("script",
								attrs=c("type"="text/javascript","charset"="utf8","src"=jslink),
								parent=headNode)}))
	
	return(htmlObj)
}

