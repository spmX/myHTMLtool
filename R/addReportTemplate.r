#' @import XML
NULL

#' @import knitr
NULL

#' @title Add default logo to the report
#' 
#' @description Add default logo (Gilead Bioinformatics) to the report
#' 
#' @param htmlObj  S3 object created by initHTML() 
#'
#' @return updated htmlObj
#'
#' @export
addLogo <- function(htmlObj)
{
	
	rootNode = XML::xmlRoot(htmlObj$htmlDoc)
	bodyNode = rootNode[["body"]]

	gildDir = "https://collaborate.gilead.com/devops/biometrics/Bioinformatics/"
	logoDir = "./Rcsslib/logo.png"
	
	#<a href="https://collaborate.gilead.com/devops/biometrics/Bioinformatics/" target="_blank">
	# <img style="border:0;position:absolute;right:45px;top:25px;z-index:9000" src="./Rcsslib/logo.png" width="103" alt="bioinfo@gilead" title="bioinfo@gilead"></a>
	logoAttrs = c(style="border:0;position:absolute;right:45px;top:25px;z-index:9000",
				width="103",
				src=logoDir,
				alt="bioinfo@gilead",
				title="bioinfo@gilead")
				
	aNode = newXMLNode("a",attrs=c(href=gildDir,target="_blank"),parent=bodyNode,at=0)
	imgNode = newXMLNode("img",attrs=logoAttrs,parent=aNode)

	
	return(htmlObj)
}


#' @title Add a report content 
#' 
#' @description Add report content (such as Introduction, Discussion) to htmlObj
#' 
#' @param report.path  path to local report file (must be written in markdown syntax)
#' @param htmlObj  S3 object created by initHTML() 
#'
#' @return updated htmlObj
#'
#' @details Report file must be written in markdown syntax so that the program can parse 
#' 	and write it into .html format. For quick start, please refer to the template file
#'  provided by this package. For more details, please go to \url{http://daringfireball.net/projects/markdown/syntax}
#' 
#' @export
addReport <- function(report.path,htmlObj) 
{
	
	#### internal functions ####
	parse.body <- function(node){

		parse.h <- function(node) {
		  xmlName(node) = "div"
		  xmlAttrs(node) = c("class"="chapter")
		  node
		  
		}

		parse.p <- function(node) {
		  xmlAttrs(node) = c("id"="docsec")
		  node
		}

		if(grepl("h",xmlName(node))) parse.h(node)
		else if(grepl("p",xmlName(node))) parse.p(node)
		else if(grepl("ul",xmlName(node))) parse.p(node)
		else if(grepl("ol",xmlName(node))) parse.p(node)
		else node
	 }
	#### end of internal functions####
	
	
	# first convert the report(in markdown syntax) to .html file
	dataDir = htmlObj$dataDir
	report.fn = basename(report.path)
	tmp.html = file.path(dataDir,sprintf("%s.html",report.fn))

	#knit2html(report.path,tmp.html,stylesheet="C:/Users/lzhuo01/Desktop/knitr_custom.css")
	knit2html(report.path,tmp.html)
	
	# then parse the report and add css style
	report = htmlTreeParse(tmp.html,trim=T,useInternalNodes=T)
	report.body = xmlRoot(report)[["body"]]
	report.child = xmlChildren(report.body)

	report.child = lapply(report.child,parse.body)
	report.wrap = newXMLNode("div",attrs=c("class"="textwrap"))
	invisible(addChildren(report.wrap,kids=report.child))
	
			
	rootNode = XML::xmlRoot(htmlObj$htmlDoc)
	bodyNode = rootNode[["body"]]
	invisible(addChildren(bodyNode,kids=c(report.wrap)))
	return(htmlObj)

}

