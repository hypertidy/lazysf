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

## Registered conditionally in .onLoad when dbplyr exports sql_dialect()
## (dbplyr >= 2.6.0). Returns a custom dialect so that:
##   - Identifier quoting uses double quotes (ANSI SQL), not backticks.
##     GDAL's SQLite engine rejects backtick-quoted identifiers from dbplyr.
##   - sql_translation() dispatches to our methods rather than
##     sql_translation.sql_dialect_sqlite (which requires RSQLite).
##   - SQLITE connections get spatial SQL translations; OGRSQL and others
##     get base translations only. This is achieved via subclassing:
##     SQLITE → c("sql_dialect_gdal_sqlite", "sql_dialect_gdal_vector", ...)
##     OGRSQL → c("sql_dialect_gdal_vector", ...)
#' @exportS3Method dbplyr sql_dialect
sql_dialect.GDALVectorConnection <- function(con, ...) {
  is_sqlite <- identical(toupper(con@dialect), "SQLITE") ||
               identical(toupper(con@dialect), "INDIRECT_SQLITE") ||
               !nzchar(con@dialect)
  d <- dbplyr::new_sql_dialect(
    dialect          = "sqlite",
    quote_identifier = function(x) dbplyr::sql_quote(x, '"'),
    has_window_clause = is_sqlite
  )
  if (is_sqlite) {
    class(d) <- c("sql_dialect_gdal_sqlite", "sql_dialect_gdal_vector", class(d))
  } else {
    class(d) <- c("sql_dialect_gdal_vector", class(d))
  }
  d
}
