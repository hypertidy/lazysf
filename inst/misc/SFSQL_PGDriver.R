## -------------------------------------------
## would go in connect.R
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
#' @param ... passed on in to [sf::st_read()]
#' @param as_tibble default override for sf::st_read (`TRUE`)
#' @param quiet default override for sf::st_read (`TRUE`)
#' @export
setMethod("dbConnect", "SFSQL_PGDriver",
          function(drv, host = "", dbname = "", user = "", password = "", readonly = TRUE, ...) {
            DSN <- glue::glue("PG:host='{host}' dbname='{dbname}' user='{user}' password='{password}'")
            new("SFSQLConnection", DSN = as.character(DSN),  readonly = readonly, ...)
          })



## ------------------------

#' Class SFSQL_PGDriver.
#'
#' SFSQL_PGDriver objects are created by [SFSQL_PG()] and used to select the correct
#' method in [dbConnect()].
#' They are a superclass of the [DBIDriver-class] class, and used purely for dispatch.
#'
#' The "Usage" section lists the class methods overridden by \pkg{lazysf}.
#'
#'
#' @keywords internal
#' @export
setClass("SFSQL_PGDriver", contains = "DBIDriver")


#' @rdname SFSQL_PGDriver-class
#' @export
setMethod("dbDataType", "SFSQL_PGDriver", function(dbObj, obj, ...) {
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

#' @rdname SFSQL_PGDriver-class
#' @export
setMethod("dbIsValid", "SFSQL_PGDriver", function(dbObj, ...) {
  TRUE
})


#' @rdname SFSQL_PGDriver-class
#' @export
setMethod("dbUnloadDriver", "SFSQL_PGDriver", function(drv, ...) {
  TRUE
})


#' @rdname SFSQL_PGDriver-class
#' @export
setMethod("dbGetInfo", "SFSQL_PGDriver",
          function(dbObj, ...) {
            #vers <- sf::sf_extSoftVersion()
            vers <- vapour::vapour_gdal_version()
            list(name = "SFSQL_PGDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = vers,
                 client.version = utils::packageVersion("lazysf"))
          })


