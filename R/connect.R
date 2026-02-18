#' @include GDALVectorConnection.R
#' @include GDALVectorDriver.R
NULL


#' GDALSQL
#'
#' GDALSQL driver, use with [DBI::dbConnect()] to open a data source readable by GDAL
#'
#' @seealso lazysf dbConnect
#' @export
#' @examples
#' GDALSQL()
GDALSQL <- function() {
  new("GDALVectorDriver")
}


#' dbConnect
#'
#' dbConnect for vector data sources readable by GDAL
#'
#' The 'OGRSQL' available is documented with GDAL:
#' \url{https://gdal.org/en/stable/user/ogr_sql_sqlite_dialect.html}
#'
#' @param drv GDALVectorDriver created by \code{GDALSQL()}
#' @param DSN  data source name, may be a file, or folder path, database
#'   connection string, or URL
#' @param readonly open in readonly mode (`TRUE` is the only option currently)
#' @param geom_format geometry output format: `"WKB"` (default), `"WKT"`,
#'   `"NONE"`, or `"BBOX"` (alias `"RCT"`). Case-insensitive.
#' @param dialect SQL dialect: `"SQLITE"` (default), `"OGRSQL"`,
#'   `"INDIRECT_SQLITE"`, or `""` (let GDAL choose). SQLITE is recommended
#'   as it supports subqueries (required for dbplyr) and spatial SQL functions.
#' @param ... ignored
#' @export
#' @examples
#' f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
#' db <- dbConnect(GDALSQL(), f)
#' dbListTables(db)
setMethod("dbConnect", "GDALVectorDriver",
          function(drv, DSN = "", readonly = TRUE,
                   geom_format = getOption("lazysf.geom_format", "WKB"),
                   dialect = getOption("lazysf.dialect", "SQLITE"),
                   ...) {
            if (nchar(DSN) < 1) {
              stop("DSN must be a valid data source name ",
                   "(file, connection string, url, ...)")
            }
            geom_format <- toupper(geom_format)
            if (geom_format == "RCT") geom_format <- "BBOX"
            geom_format <- match.arg(geom_format,
                                     c("WKB", "WKT", "NONE", "BBOX"))
            new("GDALVectorConnection",
                DSN = DSN, readonly = readonly,
                geom_format = geom_format,
                dialect = dialect, ...)
          })


#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbDisconnect", "GDALVectorConnection",
          function(conn, ...) {
            conn@DSN <- ""
            conn
          })
