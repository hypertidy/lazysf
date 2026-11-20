## batch fetch: .fetch_all() must match a single fetch(-1)

fires_gpkg <- function() {
  system.file("extdata/ynp_fires_1984_2022.gpkg", package = "gdalraster",
              mustWork = TRUE)
}

sql_layer <- function(dsn, sql) {
  lyr <- new(gdalraster::GDALVector, dsn, sql, TRUE, character(0), "", "SQLITE")
  lyr$quiet <- TRUE
  lyr
}

test_that(".fetch_all matches fetch(-1) across batch sizes", {
  skip_if_not(has_sqlite_dialect())
  sql <- "SELECT * FROM mtbs_perims ORDER BY ig_date"
  ref_lyr <- sql_layer(fires_gpkg(), sql)
  ref <- ref_lyr$fetch(-1)
  ref_lyr$close()
  class(ref) <- "data.frame"
  expect_equal(nrow(ref), 61L)

  ## 61 rows: 7 leaves a partial last batch, 61 is an exact multiple (ends
  ## with an empty fetch), 1000 is a single batch
  for (b in c(7L, 61L, 1000L)) {
    lyr <- sql_layer(fires_gpkg(), sql)
    got <- lazysf:::.fetch_all(lyr, batch = b)
    lyr$close()
    expect_s3_class(got, "data.frame")
    expect_equal(nrow(got), 61L, info = paste("batch", b))
    expect_named(got, names(ref))
    for (nm in names(ref)) {
      expect_identical(class(got[[nm]]), class(ref[[nm]]),
                       info = paste("batch", b, nm))
      expect_equal(unclass(got[[nm]]), unclass(ref[[nm]]),
                   ignore_attr = TRUE, info = paste("batch", b, nm))
    }
  }
})

test_that("collect() through batches keeps Date, integer64 and geometry", {
  skip_if_not(has_sqlite_dialect())
  d <- lazysf(fires_gpkg()) |>
    dplyr::arrange(ig_date) |>
    dplyr::collect()
  expect_equal(nrow(d), 61L)
  expect_s3_class(d$ig_date, "Date")
  expect_s3_class(d$geom, "wk_wkb")
  expect_true(all(!is.na(d$ig_date)))
})

## connection metadata cache

test_that("lazysf() and schema lookups reuse one cached handle per layer", {
  con <- DBI::dbConnect(GDALSQL(), multi_gpkg())
  on.exit(DBI::dbDisconnect(con))

  x <- lazysf(con)
  first <- DBI::dbListTables(con)[1L]
  expect_identical(con@cache$default_layer, first)
  expect_identical(ls(con@cache$layers), first)

  lyr <- lazysf:::.conn_layer(con)
  expect_identical(lazysf:::.conn_layer(con, first), lyr)

  ## repeated use does not open more handles
  lazysf(con)
  DBI::dbListFields(con, first)
  expect_identical(ls(con@cache$layers), first)

  ## a second layer gets its own handle
  second <- DBI::dbListTables(con)[2L]
  lazysf(con, second)
  expect_setequal(ls(con@cache$layers), c(first, second))
})

test_that("dbListTables is cached on the connection", {
  con <- DBI::dbConnect(GDALSQL(), multi_gpkg())
  on.exit(DBI::dbDisconnect(con))
  expect_null(con@cache$tables)
  tbls <- DBI::dbListTables(con)
  expect_identical(con@cache$tables, tbls)
})

test_that("dbDisconnect closes cached handles and clears the cache", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg())
  lazysf(con)
  lyr <- lazysf:::.conn_layer(con)
  expect_true(lyr$isOpen())
  DBI::dbDisconnect(con)
  expect_false(lyr$isOpen())
  expect_length(ls(con@cache, all.names = TRUE), 0L)
})

test_that("a closed cached handle is reopened", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  lyr <- lazysf:::.conn_layer(con, "nc")
  lyr$close()
  lyr2 <- lazysf:::.conn_layer(con, "nc")
  expect_true(lyr2$isOpen())
  expect_identical(DBI::dbListFields(con, "nc")[1:2], c("AREA", "PERIMETER"))
})
