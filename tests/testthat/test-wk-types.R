test_that("WKB column has CRS attached", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "WKB"))
  crs <- wk::wk_crs(d$geom)
  expect_false(is.null(crs))
  ## nc.gpkg uses NAD27 (EPSG:4267)
  expect_true(grepl("NAD27|4267", crs, ignore.case = TRUE))
})

test_that("WKT column has CRS attached", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "WKT"))
  crs <- wk::wk_crs(d$geom)
  expect_false(is.null(crs))
  expect_true(grepl("NAD27|4267", crs, ignore.case = TRUE))
})

test_that("BBOX column has CRS attached", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "BBOX"))
  crs <- wk::wk_crs(d$geom)
  expect_false(is.null(crs))
  expect_true(grepl("NAD27|4267", crs, ignore.case = TRUE))
})

test_that("WKB column prints without error", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg()))
  expect_output(print(d$geom[1:3]), "MULTI")
})

test_that("WKT column contains text geometry", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "WKT"))
  expect_true(all(grepl("^MULTI", as.character(d$geom[1:3]))))
})

test_that("BBOX/rct values are finite rectangles", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_gpkg(), geom_format = "BBOX"))
  r <- d$geom
  expect_true(all(is.finite(wk::rct_xmin(r))))
  expect_true(all(is.finite(wk::rct_ymin(r))))
  expect_true(all(wk::rct_xmax(r) >= wk::rct_xmin(r)))
  expect_true(all(wk::rct_ymax(r) >= wk::rct_ymin(r)))
})

test_that("schema discovery preserves wk types", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- dplyr::collect(dplyr::filter(lsf, AREA < 0.05))
  if (nrow(d) > 0) {
    expect_s3_class(d$geom, "wk_wkb")
  }
})

test_that("shapefile geometry has CRS", {
  skip_if_no_sqlite()
  d <- dplyr::collect(lazysf(nc_shp()))
  ## find the geometry column (name varies by GDAL version)
  geom_col <- NULL
  for (nm in names(d)) {
    if (inherits(d[[nm]], "wk_wkb") || inherits(d[[nm]], "wk_wkt")) {
      geom_col <- nm
      break
    }
  }
  expect_false(is.null(geom_col))
  crs <- wk::wk_crs(d[[geom_col]])
  expect_false(is.null(crs))
})
