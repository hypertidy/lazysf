#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.GDALVectorConnection <- function(con) 2L

#' @importFrom dbplyr db_connection_describe
#' @export
db_connection_describe.GDALVectorConnection <- function(con) {
  dialect <- if (nzchar(con@dialect)) con@dialect else "default"
  paste0("GDAL vector <", dialect, ">")
}
