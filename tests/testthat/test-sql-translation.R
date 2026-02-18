## Tests for SQL translation
##
## SQL *generation* tests (show_query/sql_render) work without SpatiaLite —
## they only check the SQL text that dbplyr produces.
## SQL *execution* tests require SpatiaLite and are skipped otherwise.

test_that("sql_translation returns sql_variant", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(lazysf::GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  tv <- dbplyr::sql_translation(con)
  expect_s3_class(tv, "sql_variant")
})

test_that("type casts translate correctly", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::mutate(area_int = as.integer(AREA)) |>
    dbplyr::sql_render()
  expect_match(as.character(sql), "CAST.*INTEGER", perl = TRUE)
})

test_that("logical escaping uses 0/1", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(lazysf::GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  expect_equal(dbplyr::sql_escape_logical(con, TRUE), "1")
  expect_equal(dbplyr::sql_escape_logical(con, FALSE), "0")
  expect_equal(dbplyr::sql_escape_logical(con, NA), "NULL")
})

## -- SQL generation for SpatiaLite functions (no execution) ----------------
## These test that dplyr verbs produce the right SQL text.
## They don't need SpatiaLite at runtime — sql_render is lazy.

test_that("st_area generates ST_Area SQL", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::mutate(a = st_area(geom)) |>
    dbplyr::sql_render()
  expect_match(as.character(sql), "ST_Area", fixed = TRUE)
})

test_that("st_intersects generates ST_Intersects SQL", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::filter(st_intersects(geom, geom)) |>
    dbplyr::sql_render()
  expect_match(as.character(sql), "ST_Intersects", fixed = TRUE)
})

test_that("st_buffer generates ST_Buffer SQL", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::mutate(buf = st_buffer(geom, 1000)) |>
    dbplyr::sql_render()
  expect_match(as.character(sql), "ST_Buffer", fixed = TRUE)
})

test_that("st_transform generates ST_Transform SQL", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::mutate(g2 = st_transform(geom, 3857)) |>
    dbplyr::sql_render()
  expect_match(as.character(sql), "ST_Transform", fixed = TRUE)
})

test_that("chained spatial verbs generate correct SQL", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  sql <- lf |>
    dplyr::filter(st_area(geom) > 0.1) |>
    dplyr::mutate(centroid = st_centroid(geom)) |>
    dbplyr::sql_render()
  sql_chr <- as.character(sql)
  expect_match(sql_chr, "ST_Area", fixed = TRUE)
  expect_match(sql_chr, "ST_Centroid", fixed = TRUE)
})

test_that("OGRSQL dialect omits spatial translations", {
  con <- DBI::dbConnect(lazysf::GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  ## Under OGRSQL, st_area is not translated to ST_Area —
  ## it passes through as-is (lowercase)
  sql <- dplyr::tbl(con, "nc") |>
    dplyr::mutate(a = st_area(geom)) |>
    dbplyr::sql_render()
  sql_chr <- as.character(sql)
  expect_match(sql_chr, "st_area", fixed = TRUE)
  expect_no_match(sql_chr, "ST_Area", fixed = TRUE)
})

## -- SpatiaLite execution tests (skipped without SpatiaLite) ---------------

test_that("ST_Area executes with SpatiaLite", {
  skip_if_no_spatialite()
  lf <- lazysf(nc_gpkg())
  d <- lf |>
    dplyr::mutate(computed_area = st_area(geom)) |>
    utils::head(5) |>
    dplyr::collect()
  expect_true("computed_area" %in% names(d))
  expect_true(is.numeric(d$computed_area))
})

test_that("ST_AsText executes with SpatiaLite", {
  skip_if_no_spatialite()
  lf <- lazysf(nc_gpkg())
  d <- lf |>
    dplyr::mutate(wkt = st_astext(geom)) |>
    utils::head(1) |>
    dplyr::collect()
  expect_true("wkt" %in% names(d))
  expect_match(d$wkt, "POLYGON|MULTIPOLYGON")
})

test_that("ST_Intersects filter executes with SpatiaLite", {
  skip_if_no_spatialite()
  lf <- lazysf(nc_gpkg())
  d <- lf |>
    dplyr::filter(st_intersects(geom, geom)) |>
    dplyr::collect()
  expect_gt(nrow(d), 0)
})

test_that("spatial chained operations execute with SpatiaLite", {
  skip_if_no_spatialite()
  lf <- lazysf(nc_gpkg())
  d <- lf |>
    dplyr::filter(st_area(geom) > 0) |>
    dplyr::mutate(srid = st_srid(geom)) |>
    utils::head(3) |>
    dplyr::collect()
  expect_true("srid" %in% names(d))
  expect_equal(nrow(d), 3)
})
