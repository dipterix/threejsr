#' @export
as_data_uri <- function(obj, mime = "text/plain;charset=US-ASCII", ...){
  sprintf('data:%s;base64,%s', mime, jsonlite::base64_enc(jsonlite::toJSON(obj, ...)))
}

#' @export
as_data_uri_file <- function(file, ...){
  base64enc::dataURI(file = file, ...)
}
