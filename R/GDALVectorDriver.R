
#' @import DBI
#' @import methods
#' @importMethodsFrom DBI dbDataType
NULL

#' Class GDALVectorDriver
#'
#' GDALVectorDriver objects are created by [GDALSQL()] and used to select the correct
#' method in [DBI::dbConnect()].
#' They are a superclass of the [DBI::DBIDriver-class] class, and used purely for dispatch.
#'
#' The "Usage" section lists the class methods overridden by \pkg{lazysf}.
#' The [DBI::dbUnloadDriver()] method is a null-op.
#'
#' @keywords internal
#' @export
setClass("GDALVectorDriver", contains = "DBIDriver")

#' @rdname GDALVectorDriver-class
#' @export
setMethod("dbDataType", "GDALVectorDriver", function(dbObj, obj, ...) {
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
})

#' @rdname GDALVectorDriver-class
#' @export
setMethod("dbIsValid", "GDALVectorDriver", function(dbObj, ...) {
  TRUE
})

#' @rdname GDALVectorDriver-class
#' @export
setMethod("dbUnloadDriver", "GDALVectorDriver", function(drv, ...) {
  TRUE
})

#' @rdname GDALVectorDriver-class
#' @export
setMethod("dbGetInfo", "GDALVectorDriver",
          function(dbObj, ...) {
            vers <- gdalraster::gdal_version()
            list(name = "GDALVectorDriver",
                 note = "virtual SQL driver for GDAL",
                 driver.version = vers[1L],
                 client.version = utils::packageVersion("lazysf"))
          })
