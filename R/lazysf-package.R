#' @keywords internal
#' @aliases lazysf-package
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

setOldClass(c("data.frame", "sf"))

#' lazysf package
#'
#' @section Package Options:
#' There is a debug option `options(lazysf.query.debug = TRUE)` which if set will cause the generated SQL statement
#' to be printed before every call to `sf::st_read()`. In addition it will print the number of rows actual read.
#' @name lazysf-package
NULL

#' @importFrom utils packageVersion
#' @importFrom sf sf_extSoftVersion
#' @importFrom methods setMethod setClass setOldClass callNextMethod new show
#' @importFrom DBI  dbConnect  dbSendQuery dbFetch  dbDisconnect dbClearResult dbIsValid
#'  dbHasCompleted dbReadTable dbListTables dbExistsTable dbDataType dbGetInfo dbUnloadDriver
#' @importFrom sf read_sf st_layers sf_extSoftVersion
#' @importFrom tibble tibble
NULL


# dummy imports
.use_tibble <- function() tibble::tibble()
.use_dplyr <- function() {
 dplyr::select(.use_tibble())
  NULL
}
