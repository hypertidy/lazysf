
#' @include SFSQLConnection.R
#' @include SFSQLDriver.R
NULL


#' SFSQL
#'
#' SFSQL driver, use to [dbConnect()] to a data source readable by sf
#'
#' @seealso lazysf dbConnect
#' @export
SFSQL <- function() {
  new("SFSQLDriver")
}



#' dbConnect
#'
#' dbConnect for drawings that may be read by package sf
#'
#' The 'OGRSQL' available is documented with GDAL: https://gdal.org/user/ogr_sql_dialect.html
#' @param drv SFSQLDriver created by \code{SFSQL()}
#' @param DSN  data source name, may be a file, or folder path, database connection string, or URL
#' @param readonly open in readonly mode (`TRUE` is the only option)
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' ## a nothing connection not allowed
#' ## ERR: dbConnect(SFSQL())
#' afile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' db <- dbConnect(SFSQL(), afile)
#' dbSendQuery(db, 'SELECT * FROM "nc.gpkg"')
#' }
setMethod("dbConnect", "SFSQLDriver",
          function(drv, DSN = "", readonly = TRUE, ...) {
            ## FIXME: could be a new MEM dataset
            if (nchar(DSN) < 1) stop("DSN must be a valid data source name (file, connection string, url, ...)")
            new("SFSQLConnection", DSN = DSN,  readonly = readonly, ...)
          })


#' @rdname SFSQLConnection-class
#' @export
setMethod("dbDisconnect", "SFSQLConnection",
          function(conn, ...) {
            conn@DSN <- ""
            conn
          })


