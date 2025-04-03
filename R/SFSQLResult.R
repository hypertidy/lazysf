#' Class SFSQLResult (and methods)
#'
#' SFSQLResult objects are created by [DBI::dbSendQuery()] or [DBI::dbSendStatement()],
#' and encapsulate the result of an SQL statement.
#' They are a superclass of the [DBI::DBIResult-class] class.
#' The "Usage" section lists the class methods overridden by \pkg{lazsf}.
#'
#' @seealso
#' The corresponding generic functions
#' [DBI::dbFetch()], [DBI::dbClearResult()], and
#' [DBI::dbHasCompleted()].
#'
#' @export
#' @keywords internal
setClass("SFSQLResult",
         contains = "DBIResult",
         slots = c(layer_data = "ANY")
)

# need?
# The corresponding generic functions
# [DBI::dbFetch()], [DBI::dbClearResult()], and [DBI::dbBind()],
# [DBI::dbColumnInfo()], [DBI::dbGetRowsAffected()], [DBI::dbGetRowCount()],
# [DBI::dbHasCompleted()], and [DBI::dbGetStatement()].
#


#' @rdname SFSQLResult-class
#' @export
setMethod("show", "SFSQLResult",
          function(object) {
            cat(sprintf("Field names: %s\n",
                        paste(names(object@layer_data), collapse = ", ")))
            invisible(NULL)
          })

#' @rdname SFSQLResult-class
#' @export
setMethod("dbFetch", "SFSQLResult", function(res, n = -1, ...) {
  res@layer_data
})


#' @rdname SFSQLResult-class
#' @export
setMethod("dbClearResult", "SFSQLResult", function(res, ...) {
  TRUE
})

#' @rdname SFSQLResult-class
#' @export
setMethod("dbHasCompleted", "SFSQLResult", function(res, ...) {
  TRUE
})


