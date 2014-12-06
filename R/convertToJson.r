#' @import rjson
NULL



#' @title convert input to a javascript object(json)
#'
#' @description convert input into a javascript object and write to a file
#' 
#' @param x input
#' @param ... other arguments
#' @rdname convertToJson
#' @export
convertToJson <- function(x,...) UseMethod("convertToJson")

#' @title convert to json
#'
#' @param outDir path Path to save the .json file
#' @param outPrefix character prefix of json file 
#' @param fieldnames vector Column names of the data table
#'
#' @rdname convertToJson
#' @method convertToJson default
#' @export
convertToJson.default <- function(x,outDir=".",outPrefix=NULL,fieldnames=NULL )
{
	
	# data.frame to json format, each row will become a json object
	# noted that rjson::toJSON function convet each col to a json objects
	data.frame = x
	orient="values"
	
	if(is.null(fieldnames)) fieldnames = colnames(data.frame)
	
	data.list = as.list(data.frame)
	
	#=======================================
	zipVectors <- function(..., names = F){
	  varX = list(...)
	  varY = lapply(seq_along(varX[[1]]), function(i) lapply(varX, pluck(i)))
	  if (names) names(varY) = seq_along(varY)
	  return(varY)
	}
	pluck <- function (element){
	  function(varX) varX[[element]]
	}
	#======================================
	
	data.list = switch(orient, 
						columns = data.list,
						records = do.call('zipVectors', data.list),
						values = do.call('zipVectors', setNames(data.list, fieldnames))
					   )
	df.json = rjson::toJSON(data.list)

	# write json object to a file
	
	jsonFileName = sprintf("%s.json",outPrefix)
	jsonFilePath = file.path(outDir,jsonFileName)
	jsonFile = file(jsonFilePath)
	writeLines(df.json,jsonFile)
	close(jsonFile)
	
	print(sprintf("data frame converted into .json file at %s",jsonFilePath))

    #jsonFileRelativePath = sprintf("./Rdata/%s",jsonFileName)
	return(jsonFilePath)
	
}


