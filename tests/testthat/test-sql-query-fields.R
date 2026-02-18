## Tests for sql_query_fields and schema discovery via getLayerDefn

test_that("sql_query_fields generates LIMIT 0 SQL", {
  skip_if_no_sqlite()
  con <- DBI::dbConnect(lazysf::GDALSQL(), nc_gpkg())
  on.exit(DBI::dbDisconnect(con))
  sql <- dbplyr::sql_query_fields(con, dbplyr::sql('"nc"'))
  expect_match(sql, "LIMIT 0")
  expect_match(sql, "nc")
})

test_that("field discovery returns correct column names", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  nms <- colnames(lf)
  ## Must include geometry column
  expect_true("geom" %in% nms)
  ## Must include some known NC attribute columns
  expect_true("NAME" %in% nms)
  expect_true("FIPS" %in% nms)
})

test_that("schema_from_defn builds empty typed data frame", {
  skip_if_no_sqlite()
  gpkg <- nc_gpkg()
  lyr <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr$returnGeomAs <- "WKB"
  lyr$quiet <- TRUE
  df <- lazysf:::.schema_from_defn(lyr, "WKB")
  lyr$close()

  ## Zero rows

  expect_equal(nrow(df), 0)
  ## Has columns
  expect_gt(ncol(df), 0)
  ## Geometry column has wk class
  expect_true("geom" %in% names(df))
  expect_s3_class(df$geom, "wk_wkb")
})

test_that("schema_from_defn includes FID column", {
  skip_if_no_sqlite()
  gpkg <- nc_gpkg()
  lyr <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr$returnGeomAs <- "WKB"
  lyr$quiet <- TRUE
  df <- lazysf:::.schema_from_defn(lyr, "WKB")
  lyr$close()

  ## FID or fid column should be present
  fid_cols <- grep("^[Ff][Ii][Dd]$", names(df), value = TRUE)
  expect_length(fid_cols, 1)
})

test_that("schema_from_defn column types match fetch types", {
  skip_if_no_sqlite()
  gpkg <- nc_gpkg()

  ## Schema from defn
  lyr <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr$returnGeomAs <- "WKB"
  lyr$quiet <- TRUE
  defn_df <- lazysf:::.schema_from_defn(lyr, "WKB")
  lyr$close()

  ## Schema from fetch(0) for comparison
  lyr2 <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr2$returnGeomAs <- "WKB"
  lyr2$quiet <- TRUE
  fetch_df <- lyr2$fetch(0)
  lyr2$close()

  ## Same column names (order may differ for FID/geom)
  expect_setequal(names(defn_df), names(fetch_df))
})

test_that("schema_from_defn respects geom_format NONE", {
  skip_if_no_sqlite()
  gpkg <- nc_gpkg()
  lyr <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr$returnGeomAs <- "NONE"
  lyr$quiet <- TRUE
  df <- lazysf:::.schema_from_defn(lyr, "NONE")
  lyr$close()

  ## No geometry column
  expect_false("geom" %in% names(df))
})

test_that("schema_from_defn handles WKT format", {
  skip_if_no_sqlite()
  gpkg <- nc_gpkg()
  lyr <- new(gdalraster::GDALVector, gpkg, "nc", TRUE)
  lyr$returnGeomAs <- "WKT"
  lyr$quiet <- TRUE
  df <- lazysf:::.schema_from_defn(lyr, "WKT")
  lyr$close()

  expect_true("geom" %in% names(df))
  expect_s3_class(df$geom, "wk_wkt")
})

test_that("field discovery via lazysf() uses getLayerDefn path", {
  skip_if_no_sqlite()
  ## With debug on, the schema discovery message should appear
  old <- options(lazysf.query.debug = TRUE)
  on.exit(options(old))
  expect_message(
    lazysf(nc_gpkg()),
    "schema discovery via getLayerDefn"
  )
})

test_that("schema column names match collected data", {
  skip_if_no_sqlite()
  lf <- lazysf(nc_gpkg())
  schema_names <- colnames(lf)
  data_names <- names(dplyr::collect(utils::head(lf, 1)))
  expect_equal(schema_names, data_names)
})

## -- OGR type mapping ---------------------------------------------------------

test_that(".ogr_type_to_r maps standard types correctly", {
  expect_true(is.integer(lazysf:::.ogr_type_to_r("OFTInteger")))
  expect_true(is.double(lazysf:::.ogr_type_to_r("OFTReal")))
  expect_true(is.character(lazysf:::.ogr_type_to_r("OFTString")))
  expect_true(is.logical(lazysf:::.ogr_type_to_r("OFTInteger", "OFSTBoolean")))
  expect_s3_class(lazysf:::.ogr_type_to_r("OFTDate"), "Date")
  expect_s3_class(lazysf:::.ogr_type_to_r("OFTDateTime"), "POSIXct")
  expect_true(is.list(lazysf:::.ogr_type_to_r("OFTBinary")))
})

test_that(".ogr_type_to_r falls back to character for unknown types", {
  expect_true(is.character(lazysf:::.ogr_type_to_r("OFTSomethingNew")))
})
