#' @keywords internal
#' @aliases lazysf-package
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


# dummy imports
.use_tibble <- function() tibble::tibble()
.use_dplyr <- function() {
 dplyr::select(.use_tibble())
  NULL
}
