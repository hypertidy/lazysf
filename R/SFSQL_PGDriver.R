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
            vers <- sf::sf_extSoftVersion()
            list(name = "SFSQL_PGDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = vers["GDAL"],
                 client.version = utils::packageVersion("lazysf"))
          })


