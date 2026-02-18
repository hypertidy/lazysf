#' @keywords internal
#' @aliases lazysf-package
"_PACKAGE"

#' lazysf package
#'
#' @section Package Options:
#' There is a debug option `options(lazysf.query.debug = TRUE)` which if set
#' will cause the generated SQL statement to be printed before every call.
#' In addition it will print the number of rows actually read.
#'
#' Default geometry format can be set with `options(lazysf.geom_format = "WKB")`.
#' Default SQL dialect can be set with `options(lazysf.dialect = "SQLITE")`.
#' @name lazysf-package
NULL

#' @importFrom utils packageVersion
#' @importFrom methods setMethod setClass callNextMethod new show
#' @import DBI
#' @import methods
#' @importFrom tibble tibble
#' @importFrom wk wkb wkt rct
NULL
