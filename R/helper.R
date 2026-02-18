## test data paths
nc_gpkg <- function() {
  system.file("extdata/nc.gpkg", package = "lazysf", mustWork = TRUE)
}
nc_shp <- function() {
  system.file("extdata/nc.shp", package = "lazysf", mustWork = TRUE)
}
multi_gpkg <- function() {
  system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
}
## Can we open a connection with SQLITE dialect?
## GDAL's SQLite dialect works without SpatiaLite for basic SQL,
## but some builds may not have SQLite SQL support at all.
has_sqlite_dialect <- function() {
  tryCatch({
    con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "SQLITE")
    DBI::dbDisconnect(con)
    TRUE
  }, error = function(e) FALSE)
}

skip_if_no_sqlite <- function() {
  testthat::skip_if_not(has_sqlite_dialect(), "SQLITE dialect not available")
}

## Does this GDAL build have SpatiaLite functions?
has_spatialite <- function() {
  tryCatch({
    lsf <- lazysf(nc_gpkg(),
                  query = "SELECT spatialite_version() AS v FROM nc LIMIT 1")
    d <- dplyr::collect(lsf)
    nrow(d) > 0
  }, error = function(e) FALSE)
}

skip_if_no_spatialite <- function() {
  testthat::skip_if_not(has_spatialite(), "SpatiaLite not available")
}
