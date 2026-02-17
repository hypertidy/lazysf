#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.SFSQLConnection <- function(con) 2L


#' @importFrom dbplyr db_connection_describe
#' @export
db_connection_describe.SFSQLConnection <- function(con, ...) {
  dsn <- con@DSN
  if (grepl("pass|user", dsn, ignore.case = TRUE)) {
    dsn <- paste0(strsplit(dsn, "\\s")[[1L]][1L], "...")
  }
  paste0("GDAL/OGR <", basename(dsn), ">")
}
