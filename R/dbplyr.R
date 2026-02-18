#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.GDALVectorConnection <- function(con) 2L

#' @importFrom dbplyr db_connection_describe
#' @export
db_connection_describe.GDALVectorConnection <- function(con, ...) {
  dialect <- if (nzchar(con@dialect)) con@dialect else "SQLITE"
  dsn <- con@DSN
  if (nchar(dsn) > 50) dsn <- paste0(substr(dsn, 1, 47), "...")
  paste0("GDAL <", dialect, "> ", con@geom_format, " [", dsn, "]")
}
