
#' Class SFSQLDriver.
#'
#' SFSQLDriver objects are created by [SFSQL()] and used to select the correct
#' method in [dbConnect()].
#' They are a superclass of the [DBIDriver-class] class, and used purely for dispatch.
#'
#' The "Usage" section lists the class methods overridden by \pkg{lazysf}.
#' The [dbUnloadDriver()] method is a null-op.
#'
#' @keywords internal
#' @export
setClass("SFSQLDriver", contains = "DBIDriver")



#' @rdname SFSQLDriver-class
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

#' @rdname SFSQLDriver-class
#' @export
setMethod("dbIsValid", "SFSQLDriver", function(dbObj, ...) {
  TRUE
})


#' @rdname SFSQLDriver-class
#' @export
setMethod("dbUnloadDriver", "SFSQLDriver", function(drv, ...) {
 TRUE
})


#' @rdname SFSQLDriver-class
#' @export
setMethod("dbGetInfo", "SFSQLDriver",
          function(dbObj, ...) {
            #vers <- sf::sf_extSoftVersion()
            vers <- vapour::vapour_gdal_version()
            list(name = "SFSQLDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = vers,
                 client.version = utils::packageVersion("lazysf"))
          })





