
#' File or any R objects to Data URI, base64 encoded
#' @param obj Object for file
#' @param mime Mine type
#' @param is_file obj is file or not
#' @param ... pass to jsonlite::toJSON
as_data_uri <- function(obj, mime = "text/plain;charset=US-ASCII", is_file = FALSE, ...){
  if(is_file){
    con = file(obj, "rb")
    on.exit(close(con))
    if (isTRUE(summary(con)$text == "binary")) {
      l <- raw()
      while (length(r <- readBin(con, raw(0), 1048576L))) l <- c(l, r)
      data = l
    }else{
      data = readLines(con)
      data = paste(data, collapse = '\n')
      data = jsonlite::toJSON(data)
    }
  }else{
    data = jsonlite::toJSON(obj, ...)
  }
  sprintf(
    'data:%s;base64,%s',
    mime,
    jsonlite::base64_enc(data)
  )
}


# as_data_uri <- function(obj, mime = "text/plain;charset=US-ASCII", ...){
#   sprintf('data:%s;base64,%s', mime, jsonlite::base64_enc(jsonlite::toJSON(obj, ...)))
# }

#' Convert file to dataURI via base64enc package
#' @param file file to be converted
#' @param ... pass to base64enc::dataURI
#' @export
as_data_uri_file <- function(file, ...){
  base64enc::dataURI(file = file, ...)
}
