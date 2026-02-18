test_that("st_as_sf converts WKB result to sf", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg(), geom_format = "WKB")
  sf_obj <- sf::st_as_sf(lsf)
  expect_s3_class(sf_obj, "sf")
  expect_true("geom" %in% names(sf_obj))
  expect_s3_class(sf::st_geometry(sf_obj), "sfc")
})

test_that("st_as_sf converts WKT result to sf", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg(), geom_format = "WKT")
  sf_obj <- sf::st_as_sf(lsf)
  expect_s3_class(sf_obj, "sf")
  expect_s3_class(sf::st_geometry(sf_obj), "sfc")
})

test_that("st_as_sf converts BBOX result to sf", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg(), geom_format = "BBOX")
  sf_obj <- sf::st_as_sf(lsf)
  expect_s3_class(sf_obj, "sf")
})

test_that("st_as_sf errors with geom_format NONE", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg(), geom_format = "NONE")
  expect_error(sf::st_as_sf(lsf), "geom_format")
})

test_that("st_as_sf preserves CRS", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg())
  sf_obj <- sf::st_as_sf(lsf)
  crs <- sf::st_crs(sf_obj)
  expect_false(is.na(crs))
})

test_that("st_as_sf works after dplyr verbs", {
  skip_if_no_sqlite()
  skip_if_not_installed("sf")
  lsf <- lazysf(nc_gpkg())
  sf_obj <- lsf |>
    dplyr::filter(AREA < 0.1) |>
    sf::st_as_sf()
  expect_s3_class(sf_obj, "sf")
  expect_true(all(sf_obj$AREA < 0.1))
})
