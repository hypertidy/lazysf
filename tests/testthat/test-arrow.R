## Tests for Arrow C stream fetch path
##
## These test that use_arrow = TRUE produces the same results as the
## default fetch() path.  GDAL's Arrow stream interface requires
## GDAL >= 3.6 and gdalraster with getArrowStream() support.

nc_gpkg <- function() {
  system.file("extdata/nc.gpkg", package = "lazysf")
}

has_arrow_stream <- function() {
  tryCatch({
    lyr <- new(gdalraster::GDALVector, nc_gpkg(), "nc", TRUE)
    lyr$quiet <- TRUE
    stream <- lyr$getArrowStream()
    lyr$close()
    inherits(stream, "nanoarrow_array_stream")
  }, error = function(e) FALSE)
}

skip_if_no_arrow_stream <- function() {
  testthat::skip_if_not(has_arrow_stream(),
                        "GDALVector$getArrowStream() not available")
}

## -- Arrow vs fetch: same results ---------------------------------------------

test_that("Arrow and fetch paths return same column names", {
  skip_if_no_arrow_stream()
  lf_fetch <- lazysf(nc_gpkg(), use_arrow = FALSE)
  lf_arrow <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d_fetch <- dplyr::collect(utils::head(lf_fetch, 5))
  d_arrow <- dplyr::collect(utils::head(lf_arrow, 5))
  expect_equal(names(d_arrow), names(d_fetch))
})

test_that("Arrow and fetch paths return same nrows", {
  skip_if_no_arrow_stream()
  lf_arrow <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- dplyr::collect(lf_arrow)
  expect_equal(nrow(d), 100)
})

test_that("Arrow path returns wk_wkb geometry by default", {
  skip_if_no_arrow_stream()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- dplyr::collect(utils::head(lf, 3))
  expect_true("geom" %in% names(d))
  expect_s3_class(d$geom, "wk_wkb")
})

test_that("Arrow path respects geom_format = NONE", {
  skip_if_no_arrow_stream()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE, geom_format = "NONE")
  d <- dplyr::collect(utils::head(lf, 3))
  expect_false("geom" %in% names(d))
})

test_that("Arrow path respects geom_format = WKT", {
  skip_if_no_arrow_stream()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE, geom_format = "WKT")
  d <- dplyr::collect(utils::head(lf, 3))
  expect_true("geom" %in% names(d))
  expect_s3_class(d$geom, "wk_wkt")
})

## -- dplyr verbs with Arrow ---------------------------------------------------

test_that("Arrow: filter works", {
  skip_if_no_arrow_stream()
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- lf |>
    dplyr::filter(AREA < 0.1) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.1))
})

test_that("Arrow: select works", {
  skip_if_no_arrow_stream()
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- lf |>
    dplyr::select(NAME, AREA) |>
    dplyr::collect()
  expect_true(all(c("NAME", "AREA") %in% names(d)))
})

test_that("Arrow: rename works", {
  skip_if_no_arrow_stream()
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- lf |>
    dplyr::rename(county = NAME) |>
    utils::head(3) |>
    dplyr::collect()
  expect_true("county" %in% names(d))
})

test_that("Arrow: arrange works", {
  skip_if_no_arrow_stream()
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- lf |>
    dplyr::arrange(AREA) |>
    dplyr::collect()
  expect_true(all(diff(d$AREA) >= 0))
})

test_that("Arrow: chained verbs work", {
  skip_if_no_arrow_stream()
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg(), use_arrow = TRUE)
  d <- lf |>
    dplyr::filter(AREA < 0.15) |>
    dplyr::select(NAME, AREA, geom) |>
    dplyr::arrange(dplyr::desc(AREA)) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.15))
})

## -- Connection display -------------------------------------------------------

test_that("show() displays arrow status", {
  skip_if_no_arrow_stream()
  con <- DBI::dbConnect(lazysf::GDALSQL(), nc_gpkg(), use_arrow = TRUE)
  out <- capture.output(show(con))
  expect_true(any(grepl("arrow.*on", out)))
  DBI::dbDisconnect(con)
})
