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

## Registered conditionally in .onLoad when dbplyr supports sql_dialect().
## Returns SQLite dialect which matches our default SQL execution via
## GDAL's SQLite dialect. Harmless on current dbplyr (2.5.x) which
## doesn't call sql_dialect(); activates on future dbplyr (2.6+).
sql_dialect.GDALVectorConnection <- function(con, ...) {
  dbplyr::dialect_sqlite()
}
