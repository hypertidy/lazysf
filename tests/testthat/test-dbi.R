test_that("dbListTables returns layer names", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  tbls <- DBI::dbListTables(con)
  expect_true("nc" %in% tbls)
})

test_that("dbListFields returns column names", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  flds <- DBI::dbListFields(con, "nc")
  expect_true("AREA" %in% flds)
  expect_true("NAME" %in% flds)
  expect_true("geom" %in% flds)
})

test_that("dbExistsTable works", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbExistsTable(con, "nc"))
  expect_false(DBI::dbExistsTable(con, "nonexistent"))
})

test_that("dbGetInfo returns expected fields", {
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg(), dialect = "OGRSQL")
  on.exit(DBI::dbDisconnect(con))
  info <- DBI::dbGetInfo(con)
  expect_true("dbname" %in% names(info))
  expect_true("db.version" %in% names(info))
})

test_that("dbReadTable retrieves full table", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbReadTable(con, "nc")
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0)
  expect_true("AREA" %in% names(d))
})
