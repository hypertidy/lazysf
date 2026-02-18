# dbplyr field discovery via GDAL layer metadata
#
# sql_query_fields() generates LIMIT 0 SQL for field discovery.
# In dbSendQuery(), this is intercepted for named tables and the
# schema is built from $getLayerDefn() without executing SQL —
# pure OGR metadata access.  For subqueries, the LIMIT 0 SQL
# executes normally (SQLITE dialect handles it natively).

#' @importFrom dbplyr sql_query_fields
#' @export
sql_query_fields.GDALVectorConnection <- function(con, sql, ...) {
  ## Generate LIMIT 0 instead of the default WHERE (0 = 1).
  ## sql is either a quoted table name (e.g. `"nc"`) or a SQL statement.
  ## Subqueries must be wrapped in parentheses with an alias.
  sql_chr <- as.character(sql)
  if (grepl("^\\s*SELECT\\s", sql_chr, ignore.case = TRUE)) {
    paste0("SELECT * FROM (", sql_chr, ") AS _q LIMIT 0")
  } else {
    paste0("SELECT * FROM ", sql_chr, " LIMIT 0")
  }
}

## Build an empty data frame from OGR layer definition.
## Uses $getLayerDefn() — no SQL execution, no fetch, just OGR metadata.
## Maps OGR field types to R types so the schema matches what fetch() returns.
.schema_from_defn <- function(lyr, geom_format) {
  defn <- lyr$getLayerDefn()
  geom_info <- .geom_info(lyr)

  ## FID column — fetch() always returns this as uppercase "FID",

  ## even when getFIDColumn() reports lowercase (e.g. GeoPackage "fid").
  fid_name <- lyr$getFIDColumn()
  if (!nzchar(fid_name)) fid_name <- "FID" else fid_name <- toupper(fid_name)
  cols <- stats::setNames(list(integer(0L)), fid_name)

  ## Attribute columns from OGR field definitions.
  ## Exclude the geometry column — getLayerDefn() includes it but
  ## fetch() handles geometry separately via returnGeomAs.
  attr_names <- setdiff(names(defn), geom_info$col)
  for (nm in attr_names) {
    cols[[nm]] <- .ogr_type_to_r(defn[[nm]]$type, defn[[nm]]$subtype)
  }

  df <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)

  ## Geometry column with wk type marking
  if (geom_format != "NONE" && nzchar(geom_info$col)) {
    df[[geom_info$col]] <- .empty_geom_col(geom_format, geom_info$crs)
  }

  df
}

## Map OGR field type/subtype to empty R vector of the correct type.
## These match the types returned by GDALVector$fetch().
.ogr_type_to_r <- function(type, subtype = "") {
  ## Handle Boolean subtype (stored as OFTInteger in OGR)
  if (identical(subtype, "OFSTBoolean")) return(logical(0L))

  switch(type,
         "OFTInteger"       = integer(0L),
         "OFTInteger64"     = if (requireNamespace("bit64", quietly = TRUE)) {
           bit64::integer64(0L)
         } else {
           double(0L)
         },
         "OFTReal"          = double(0L),
         "OFTString"        = character(0L),
         "OFTBinary"        = list(),
         "OFTDate"          = as.Date(character(0L)),
         "OFTTime"          = character(0L),
         "OFTDateTime"      = as.POSIXct(character(0L)),
         "OFTIntegerList"   = list(),
         "OFTInteger64List" = list(),
         "OFTRealList"      = list(),
         "OFTStringList"    = list(),
         ## Fallback for unknown types
         character(0L)
  )
}

## Create an empty geometry column with the correct wk type.
.empty_geom_col <- function(geom_format, crs) {
  switch(toupper(geom_format),
         "WKB"     = , "WKB_ISO" = wk::wkb(list(), crs = crs),
         "WKT"     = , "WKT_ISO" = wk::wkt(character(0L), crs = crs),
         "BBOX"    = wk::rct(crs = crs),
         raw(0L)
  )
}
