.onLoad <- function(libname, pkgname) {
  op <- getOption("lazysf.query.debug")
  if (is.null(op) || is.na(op) || length(op) < 1) {
    options(lazysf.query.debug = FALSE)
  }

  ## register st_as_sf method if sf is available
  if (requireNamespace("sf", quietly = TRUE)) {
    register_s3_method("sf", "st_as_sf", "tbl_GDALVectorConnection")
  }

  invisible()
}

## from vctrs, register an S3 method only if the generic's package is available
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1L)
  stopifnot(is.character(generic), length(generic) == 1L)
  stopifnot(is.character(class), length(class) == 1L)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class),
               envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  ## register a hook for when the package is loaded later
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
