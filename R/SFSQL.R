setOldClass(c("data.frame", "sf"))


# importFrom DBI dbUnloadDriver

#' Driver for SFSQL.
#'
#' @keywords internal
#' @export
#' @importFrom DBI  dbConnect  dbSendQuery dbFetch  dbDisconnect dbClearResult
#'  dbHasCompleted dbReadTable dbListTables dbExistsTable dbDataType dbGetInfo
#' @importFrom methods setMethod setClass setOldClass callNextMethod new show
#' @importFrom sf read_sf st_layers sf_extSoftVersion
#' @importFrom tibble tibble
setClass("SFSQLDriver", contains = "DBIDriver")
setClass("SFSQL_PGDriver", contains = "DBIDriver")




#' SFSQL
#'
#' SFSQL driver
#' https://gdal.org/user/ogr_sql_dialect.html
#' @export
SFSQL <- function() {
  new("SFSQLDriver")
}

#' SFSQL_PG
#'
#' SFSQL_PG driver, a wrapper for SFSQL to construct the full connection string required by
#' GDAL from host,dbname,user,password
#' @export
#' @name SFSQL
SFSQL_PG <- function() {
  new("SFSQL_PGDriver")
}

#' SFSQL connection class.
#' @rdname SFSQLConnection-class
#' @export
#' @keywords internal
setClass("SFSQLConnection",
         contains = "DBIConnection",
         slots = list(
           DSN = "character",
           readonly = "logical",
           sf_read_args = "list")
)


#' @rdname SFSQLConnection-class
#' @export
setMethod("show", "SFSQLConnection", function(object) {
  cat("<SFSQLConnection>\n")
  tables <- DBI::dbListTables(object)
  dsn <- object@DSN
  if (grepl("pass", dsn, ignore.case = TRUE) || grepl("user", dsn, ignore.case = TRUE)) {

    dsn <- paste0(strsplit(dsn, "\\s")[[1L]][1L], "...")
  }
  cat("   DSN: ", dsn, "\n", sep = "")
  cat("tables: ", paste(tables, collapse = ", "), "\n", sep = "")
})
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

#' show
#'
#' show for SFSQLDriver
#' @param object SFSQLDriver
#' @export
setMethod("show", "SFSQLDriver", function(object) {
  cat("<SFSQLDriver>\n")
})

#' dbDisconnect
#'
#' dbDisconnect for SFSQLConnection
#' @param conn SFSQLConnection
#' @param ... ignored
#' @export
setMethod("dbDisconnect", "SFSQLConnection",
          function(conn, ...) {
            conn@DSN <- ""
            conn
          })


#' SFSQL results class
#'
#' @keywords internal
#' @export
setClass("SFSQLResult",
         contains = "DBIResult",
         slots = c(layer_data = "ANY")
)

#' Send a query to SFSQL.
#'
#' @param conn database connection, s created by \code{\link{dbConnect}}
#' @param statement OGR SQL, see http://www.gdal.org/ogr_sql.html
#' @param ... for compatibility with generic
#' @export
#' @examples
#' afile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' db <- dbConnect(SFSQL(), afile)
#' dbSendQuery(db, "SELECT * FROM \"nc.gpkg\" WHERE FID < 1")
setMethod("dbSendQuery", "SFSQLConnection",
          function(conn, statement, ...) {
            ## quiet and fake layer because we aren't using layer  = (it's in the query)
            args <- conn@sf_read_args
            args$dsn <- DSN <- conn@DSN

            args$query <- statement           ## user can't do this (warn?)
            layer_data <- do.call(sf::st_read, args)

            #layer_data <- sf::read_sf(DSN, layer = "<this is unused>", query = statement, quiet = TRUE)
            if (inherits(layer_data, "try-error")) {
              message("executing SQL failed:")
              writeLines(statement)
              if (length(gregexpr("SELECT", statement, ignore.case = TRUE)[[1]]) > 1) {
                stop("perhaps driver in use does not support sub-queries?")
              } else {
                stop("")
              }
            }
            new("SFSQLResult",
                layer_data = layer_data)

          })


#' show
#'
#' show for SFSQLResult
#' @param object SFSQLResult
#' @importFrom utils head
#' @export
setMethod("show", "SFSQLResult",
          function(object) {
            cat(sprintf("Field names: %s\n",
                        paste(names(object@layer_data), collapse = ", ")))
            invisible(NULL)
          })
#' dbFetch
#'
#' Retrieve records from SFSQL query
#'
#' @param res An object inheriting from `DBIResult`, created by `dbSendQuery()`
#' @param n maximum number of records to retrieve per fetch. Use n = -1 or n = Inf to retrieve all pending records.
#' @param ... Other arguments passed on to methods.
#' @export
setMethod("dbFetch", "SFSQLResult", function(res, n = -1, ...) {
  res@layer_data
})




#' dbReadTable
#'
#' dbReadTable for SFSQLConnection
#'
#' sends a query and fetches the result
#' @param conn SFSQLConnnection
#' @param name table name ('layer' in GDAL)
#' @param ... ignored
#' @export
setMethod("dbReadTable", c(conn = "SFSQLConnection", name = "character"),
          function(conn, name, ...){
            x <- dbSendQuery(conn, sprintf("SELECT * FROM %s", name))
            dbFetch(x)
          })

#' dbListTables
#'
#' dbListTables for SFSQLConnection
#'
#' @param conn SFSQLConnnection
#' @param ... arguments to [sf::st_layers()]
#' @export
setMethod("dbListTables", c(conn = "SFSQLConnection"),
          function(conn, ...){
            layers <- sf::st_layers(conn@DSN, ...)
            layers$name
          })

#' dbExistsTable
#'
#' dbExistsTable for SFSQLConnection
#'
#' @param conn SFSQLConnnection
#' @param name table name ('layer' in GDAL)
#' @param ... arguments to generic
#' @export
setMethod("dbExistsTable", c(conn = "SFSQLConnection"),
          function(conn, name, ...){
            name %in% dbListTables(conn, ...)
          })


#' dbDataType
#'
#' dbDataType for SFSQLDriver
#'
#' @param dbObj SFSQLConnnection
#' @param obj object
#' @param ... ignored
#' @export
setMethod("dbDataType", "SFSQLDriver", function(dbObj, obj, ...) {
  ## see "type of the fields" http://www.gdal.org/ogr_sql.html
  if (is.factor(obj)) return("character")
  if (is.data.frame(obj)) return(callNextMethod(dbObj, obj))

  switch(typeof(obj),
         logical = "boolean",
         character = "character",
         double = "numeric",
         integer = "integer",
         list = "character",
         raw = "character",
         blob = "character",
         stop("Unsupported type", call. = FALSE)
  )
}


)

#' dbGetInfo
#'
#' dbGetInfo for SFSQLDriver
#'
#' @param dbObj SFSQLDriver
#' @param ... ignored
#' @export
#' @importFrom utils packageVersion
#' @importFrom sf sf_extSoftVersion
setMethod("dbGetInfo", "SFSQLDriver",
          function(dbObj, ...) {
            vers <- sf::sf_extSoftVersion()
            list(name = "SFSQLDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = vers["GDAL"],
                 client.version = utils::packageVersion("lazysf"))
          })

# ---- This seemingly doesn't do anything but must exist for the Result class

#' dbClearResult
#'
#' dbClearResult for SFSQLResult
#'

#' @param res SFSQLResult
#' @param ... ignored
#' @export
setMethod("dbClearResult", "SFSQLResult", function(res, ...) {
  TRUE
})

#' dbHasCompleted
#'
#' dbHasCompleted for SFSQLResult
#'
#' This doesn't do anything
#' @param res SFSQLResult
#' @param ... ignored
#' @export
setMethod("dbHasCompleted", "SFSQLResult", function(res, ...) {
  TRUE
})

## ------------------------------


## ------------------------------

## things that do nothing and don't seem needed below

## @export
## @rdname SFSQLDriver-class
# setMethod("dbUnloadDriver", "SFSQLDriver", function(drv, ...) {
#   TRUE
# })

## ------------------------------




