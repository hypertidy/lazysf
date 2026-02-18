test_that("lazysf() creates a lazy tbl", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  expect_s3_class(lsf, "tbl_GDALVectorConnection")
  expect_s3_class(lsf, "tbl_lazy")
})

test_that("dbConnect/dbDisconnect round-trip", {
  ## OGRSQL always available, no SQLITE needed
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  expect_s4_class(con, "GDALVectorConnection")
  expect_true(DBI::dbIsValid(con))
  ## dbDisconnect is a no-op for validity (S4 slots are copy-on-modify)
  ## but should not error
  expect_no_error(DBI::dbDisconnect(con))
})

test_that("lazysf() accepts layer argument", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), layer = "nc")
  expect_s3_class(lsf, "tbl_GDALVectorConnection")
  d <- dplyr::collect(lsf)
  expect_gt(nrow(d), 0)
})

test_that("lazysf() passes ... to dbConnect", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), geom_format = "WKT")
  d <- dplyr::collect(lsf)
  expect_s3_class(d$geom, "wk_wkt")
})

test_that("shapefile connection works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_shp())
  expect_s3_class(lsf, "tbl_GDALVectorConnection")
  d <- dplyr::collect(lsf)
  expect_gt(nrow(d), 0)
})

test_that("multi-layer gpkg defaults to first layer", {
  skip_if_no_sqlite()
  lsf <- lazysf(multi_gpkg())
  expect_s3_class(lsf, "tbl_GDALVectorConnection")
})
