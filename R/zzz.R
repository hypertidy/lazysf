.onLoad <- function(...) {
  op <- getOption("lazysf.query.debug")
  if (is.null(op) || is.na(op) || length(op) < 1) {
    op <- FALSE ## options(lazysf.query.debug = TRUE)
    options(lazysf.query.debug = op)
  }

  invisible()
}
