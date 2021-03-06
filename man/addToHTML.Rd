% Generated by roxygen2 (4.0.2): do not edit by hand
\name{addDataFrameToHTML}
\alias{addDataFrameToHTML}
\alias{addFigureToHTML}
\alias{addJsonFileToHTML}
\title{Add data frame to HTML file}
\usage{
addDataFrameToHTML(dataframe, htmlObj, customFilter = "auto",
  tableID = NULL, fieldnames = NULL)

addJsonFileToHTML(jsonFilePath, htmlObj, fieldnames, customFilter = NULL,
  tableID = NULL)

addFigureToHTML(figurePath, htmlObj, figureID = NULL)
}
\arguments{
\item{dataframe}{data.frame R data frame containg table data to be added to the report}

\item{htmlObj}{S3 object created by initHTML()}

\item{customFilter}{vector  User-defined column filtering criteria. See Details.}

\item{tableID}{character id of the data table to be added to html file}

\item{fieldnames}{vector Field names of data frame}

\item{jsonFilePath}{Path to .json file containing table data}

\item{figurePath}{Path to the figure}

\item{figureID}{character id of the figure to be added to html file}
}
\value{
.html file Report file containing data table

htmlObj The updated htmlObj object containing the data table and javascripts required for parsing the table
}
\description{
Add table data stored as R data.frame format to a report file in HTML format
}
\details{
When customFilter is set to "auto",it automatically create individual column filters
 based on data type of each col. For instance, if a column contains characters, it's column filter
 will be a search box ; if a column contains  numbers,it's column filter will be range filter;
 User can also specify which specific column(s) need to be filter.
 For example, if user wants to set the 4th column a five-column table to be filtered by keyword search,
 simply set:
     customFilter = c("null","null","null","search","null")
 The output of this function is a .html file, along with required css and javascript library, as well as
 a data folder containing the .json file.
}
\examples{
outDir = "."
data(gwas)
data(DEgene)
htmlObj = initHTML(title="Report Title",outDir=outDir,outPrefix="Example",author="Author Name")
htmlObj = addLogo(htmlObj)
# use report template
template = system.file("extdata/rmarkdown/report.template.rmd",package="myHTMLtool")
htmlObj = addReport(template,htmlObj)
# add data to the report, noted that tableID should be the same as table id specified in report template
htmlObj = addDataFrameToHTML(dataframe=gwas.df,htmlObj=htmlObj,customFilter="auto",tableID="mytable1")
htmlObj = addDataFrameToHTML(dataframe=DEgene.df,htmlObj=htmlObj,customFilter="auto",tableID="mytable2")
}

