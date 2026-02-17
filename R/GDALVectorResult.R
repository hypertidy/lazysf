#' @importClassesFrom DBI DBIResult
NULL


#' Class GDALVectorResult (and methods)
#'
#' GDALVectorResult objects are created by [DBI::dbSendQuery()],
#' and encapsulate the result of an SQL statement.
#' They are a superclass of the [DBI::DBIResult-class] class.
#' The "Usage" section lists the class methods overridden by \pkg{lazysf}.
#'
#' @seealso
#' The corresponding generic functions
#' [DBI::dbFetch()], [DBI::dbClearResult()], and
#' [DBI::dbHasCompleted()].
#'
#' @export
#' @keywords internal
setClass("GDALVectorResult",
         contains = "DBIResult",
         slots = c(layer_data = "ANY")
)


#' @rdname GDALVectorResult-class
#' @export
setMethod("show", "GDALVectorResult",
          function(object) {
            cat(sprintf("Field names: %s\n",
                        paste(names(object@layer_data), collapse = ", ")))
            invisible(NULL)
          })

#' @rdname GDALVectorResult-class
#' @export
setMethod("dbFetch", "GDALVectorResult", function(res, n = -1, ...) {
  res@layer_data
})


#' @rdname GDALVectorResult-class
#' @export
setMethod("dbClearResult", "GDALVectorResult", function(res, ...) {
  TRUE
})

#' @rdname GDALVectorResult-class
#' @export
setMethod("dbHasCompleted", "GDALVectorResult", function(res, ...) {
  TRUE
})
