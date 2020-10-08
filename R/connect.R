
#' @include SFSQLConnection.R
#' @include SFSQLDriver.R
#' @include SFSQL_PGDriver.R
NULL


#' SFSQL
#'
#' SFSQL driver
#' https://gdal.org/user/ogr_sql_dialect.html
#' @export
SFSQL <- function() {
  new("SFSQLDriver")
}



#' dbConnect
#'
#' dbConnect
#'
#' https://gdal.org/user/ogr_sql_dialect.html
#' @param drv SFSQLDriver created by \code{SFSQL()}
#' @param DSN  data source name, may be a file, or folder path, database connection string, or URL
#' @param readonly open in readonly mode?
#' @param ... unused
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





#' SFSQL_PG
#'
#' SFSQL_PG driver, a wrapper for SFSQL to construct the full connection string required by
#' GDAL from host,dbname,user,password
#' @export
#' @name SFSQL_PG
SFSQL_PG <- function() {
  new("SFSQL_PGDriver")
}


#' dbConnect
#'
#' dbConnect for PostgreSQL via GDAL
#'
#' https://gdal.org/drivers/vector/pg.html
#' @param drv SFSQL_PGDriver created by \code{SFSQL_PG()}
#' @param host database server
#' @param dbname database name
#' @param user user name if needed
#' @param password password if needed
#' @param readonly open in readonly mode?
#' @param ... unused
#' @export
setMethod("dbConnect", "SFSQL_PGDriver",
          function(drv, host = "", dbname = "", user = "", password = "", readonly = TRUE, ...) {
            DSN <- glue::glue("PG:host='{host}' dbname='{dbname}' user='{user}' password='{password}'")
            new("SFSQLConnection", DSN = as.character(DSN),  readonly = readonly, ...)
          })


#' @rdname SFSQLConnection-class
#' @export
setMethod("dbDisconnect", "SFSQLConnection",
          function(conn, ...) {
            conn@DSN <- ""
            conn
          })
