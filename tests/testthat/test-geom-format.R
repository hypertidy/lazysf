test_that("geom_format WKB produces wk_wkb column", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), geom_format = "WKB")
  d <- dplyr::collect(lsf)
  expect_true("geom" %in% names(d))
  expect_s3_class(d$geom, "wk_wkb")
})

test_that("geom_format WKT produces wk_wkt column", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), geom_format = "WKT")
  d <- dplyr::collect(lsf)
  expect_true("geom" %in% names(d))
  expect_s3_class(d$geom, "wk_wkt")
})

test_that("geom_format BBOX produces wk_rct column", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), geom_format = "BBOX")
  d <- dplyr::collect(lsf)
  expect_true("geom" %in% names(d))
  expect_s3_class(d$geom, "wk_rct")
})

test_that("geom_format NONE omits geometry", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg(), geom_format = "NONE")
  d <- dplyr::collect(lsf)
  is_geom <- vapply(d, function(col) {
    inherits(col, "wk_wkb") || inherits(col, "wk_wkt") || inherits(col, "wk_rct")
  }, logical(1))
  expect_false(any(is_geom))
})

test_that("geom_format is case-insensitive", {
  skip_if_no_sqlite()
  d_lower <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "wkb"))
  d_upper <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "WKB"))
  expect_s3_class(d_lower$geom, "wk_wkb")
  expect_s3_class(d_upper$geom, "wk_wkb")

  d_wkt <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "wkt"))
  expect_s3_class(d_wkt$geom, "wk_wkt")
})

test_that("RCT/rct alias maps to BBOX", {
  skip_if_no_sqlite()
  d_rct <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "rct"))
  d_bbox <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "BBOX"))
  expect_s3_class(d_rct$geom, "wk_rct")
  expect_s3_class(d_bbox$geom, "wk_rct")
})

test_that("invalid geom_format errors", {
  expect_error(lazysf(nc_gpkg(), geom_format = "INVALID"))
})
