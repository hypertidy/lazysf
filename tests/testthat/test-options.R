test_that("default dialect is SQLITE", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  expect_equal(con@dialect, "SQLITE")
})

test_that("dialect option is respected", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  expect_equal(con@dialect, "OGRSQL")
})

test_that("direct SQL query works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(),
                 query = "SELECT AREA, NAME, geom FROM nc WHERE AREA < 0.1")
  d <- dplyr::collect(lsf)
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.1))
  expect_true(all(c("AREA", "NAME", "geom") %in% names(d)))
})

test_that("global option lazysf.geom_format is respected", {
  skip_if_no_sqlite()
  old <- getOption("lazysf.geom_format")
  on.exit(options(lazysf.geom_format = old))
  options(lazysf.geom_format = "WKT")
  lsf <- lazysf(nc_gpkg())
  d <- dplyr::collect(lsf)
  expect_s3_class(d$geom, "wk_wkt")
})

test_that("global option lazysf.dialect is respected", {
  old <- getOption("lazysf.dialect")
  on.exit(options(lazysf.dialect = old))
  options(lazysf.dialect = "OGRSQL")
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(con@dialect, "OGRSQL")
})
