test_that("dplyr::filter generates valid SQL", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  filtered <- dplyr::filter(lsf, AREA < 0.1)
  d <- dplyr::collect(filtered)
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.1))
})

test_that("dplyr::select subsets columns", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  selected <- dplyr::select(lsf, NAME, AREA)
  d <- dplyr::collect(selected)
  expect_true("NAME" %in% names(d))
  expect_true("AREA" %in% names(d))
})

test_that("dplyr::arrange orders rows", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  ordered <- dplyr::arrange(lsf, AREA)
  d <- dplyr::collect(ordered)
  expect_true(all(diff(d$AREA) >= 0))
})

test_that("dplyr::mutate adds computed column", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  mutated <- dplyr::mutate(lsf, area2 = AREA * 2)
  d <- dplyr::collect(mutated)
  expect_true("area2" %in% names(d))
  expect_equal(d$area2, d$AREA * 2)
})

test_that("show_query returns SQL", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  q <- dplyr::show_query(dplyr::filter(lsf, AREA < 0.1))
  expect_true(inherits(q, "tbl_sql") || is.character(q))
})

test_that("db_connection_describe includes geom_format", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), geom_format = "WKT")
  on.exit(DBI::dbDisconnect(con))
  desc <- dbplyr::db_connection_describe(con)
  expect_true(grepl("WKT", desc))
  expect_true(grepl("GDAL", desc))
  expect_true(grepl("SQLITE", desc))
})

test_that("chained verbs produce correct results", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::filter(AREA < 0.15) |>
    dplyr::select(NAME, AREA, geom) |>
    dplyr::arrange(dplyr::desc(AREA)) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.15))
  expect_true(all(diff(d$AREA) <= 0))
  expect_true(all(c("NAME", "AREA", "geom") %in% names(d)))
})

test_that("head/limit works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- dplyr::collect(utils::head(lsf, 5))
  expect_equal(nrow(d), 5)
})

test_that("dplyr::summarise computes aggregates", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::summarise(
      mean_area = mean(AREA, na.rm = TRUE),
      n = dplyr::n()
    ) |>
    dplyr::collect()
  expect_equal(nrow(d), 1)
  expect_true(is.numeric(d$mean_area))
  expect_true(d$n > 0)
})

test_that("dplyr::group_by + summarise works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::group_by(SID74) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::collect()
  expect_gt(nrow(d), 1)
  expect_true(all(c("SID74", "n") %in% names(d)))
})

test_that("dplyr::distinct works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::distinct(SID74) |>
    dplyr::collect()
  expect_equal(nrow(d), length(unique(d$SID74)))
})

test_that("dplyr::count works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::count(SID74) |>
    dplyr::collect()
  expect_true(all(c("SID74", "n") %in% names(d)))
  expect_gt(nrow(d), 1)
})

test_that("dplyr::rename works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::rename(county = NAME) |>
    utils::head(3) |>
    dplyr::collect()
  expect_true("county" %in% names(d))
  expect_false("NAME" %in% names(d))
})

test_that("dplyr::pull extracts a column", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  nms <- lsf |>
    utils::head(5) |>
    dplyr::pull(NAME)
  expect_true(is.character(nms))
  expect_length(nms, 5)
})

test_that("dplyr::transmute works", {
  skip_if_no_sqlite()
  lsf <- lazysf(nc_gpkg())
  d <- lsf |>
    dplyr::transmute(county = NAME, big = AREA > 0.1) |>
    utils::head(5) |>
    dplyr::collect()
  expect_true(all(c("county", "big") %in% names(d)))
  ## FID is always included by GDAL, so 3 columns not 2
  expect_equal(ncol(d), 3L)
})

## -- OGRSQL dialect tests -----------------------------------------------------
## These confirm the same verbs work through GDAL's native OGR SQL engine.

test_that("OGRSQL: filter works", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    dplyr::filter(AREA < 0.1) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.1))
})

test_that("OGRSQL: select works", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    dplyr::select(NAME, AREA) |>
    dplyr::collect()
  expect_true(all(c("NAME", "AREA") %in% names(d)))
})

test_that("OGRSQL: mutate works", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    dplyr::mutate(area2 = AREA * 2) |>
    utils::head(5) |>
    dplyr::collect()
  expect_true("area2" %in% names(d))
  expect_equal(d$area2, d$AREA * 2)
})

test_that("OGRSQL: arrange works", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    dplyr::arrange(AREA) |>
    dplyr::collect()
  expect_true(all(diff(d$AREA) >= 0))
})

test_that("OGRSQL: head/limit works", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    utils::head(5) |>
    dplyr::collect()
  expect_equal(nrow(d), 5)
})

test_that("OGRSQL: chained verbs work", {
  lsf <- lazysf(nc_gpkg(), dialect = "OGRSQL")
  d <- lsf |>
    dplyr::filter(AREA < 0.15) |>
    dplyr::select(NAME, AREA, geom) |>
    dplyr::arrange(dplyr::desc(AREA)) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
  expect_true(all(d$AREA < 0.15))
  expect_true(all(c("NAME", "AREA", "geom") %in% names(d)))
})
