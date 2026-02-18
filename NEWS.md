# lazysf (dev)

## Breaking changes

* The backend now uses gdalraster for all GDAL access. sf moves
from Imports to Suggests and is only used for `st_as_sf()`.

* DBI class names renamed: `SFSQLConnection` → `GDALVectorConnection`,
  `SFSQLDriver` → `GDALVectorDriver`, `SFSQLResult` → `GDALVectorResult`.
  The driver constructor is now `GDALSQL()` (was `SFSQL()`).

* The magrittr pipe (`%>%`) is no longer re-exported. Use R's native pipe (`|>`).

## New features

* New `geom_format` argument to `dbConnect()` controls geometry output:
  `"WKB"` (default), `"WKT"`, `"NONE"`, or `"BBOX"`. Configurable globally
  via `options(lazysf.geom_format = ...)`.

* New `dialect` argument to `dbConnect()` controls SQL dialect:
  `"SQLITE"` (the default), `"OGRSQL"`, `"INDIRECT_SQLITE"`, or `""` (let GDAL
  choose). SQLITE is the default because it supports subqueries (required for
  dbplyr verb chaining) and spatial SQL functions like `ST_Area()`.
  Configurable globally via `options(lazysf.dialect = ...)`.

* `lazysf()` now passes `...` through to `dbConnect()`, so `geom_format` and
  `dialect` can be set directly: `lazysf(dsn, geom_format = "WKT")`.

* Added `db_connection_describe()` method for informative printing:
  `GDAL <SQLITE> WKB [/path/to/file.gpkg]`.

* New `sql_query_fields()` method generates `LIMIT 0` SQL for field discovery
  (replacing dbplyr's default `WHERE (0 = 1)` which OGRSQL can't handle).
  For named tables, `dbSendQuery()` intercepts this and uses
  `$getLayerDefn()` for pure OGR metadata access — no SQL execution at all.
  Subqueries are correctly wrapped in parentheses with an alias.

* New `.schema_from_defn()` builds typed empty data frames from OGR layer
  definitions, mapping OGR field types to R types (`OFTInteger` → `integer`,
  `OFTReal` → `double`, `OFTDateTime` → `POSIXct`, `OFSTBoolean` → `logical`,
  etc.). Schema column names and types match what `fetch()` returns.

* New `dbListFields()` method uses `$getLayerDefn()` for schema introspection
  without executing SQL.

* Geometry columns are now automatically marked with wk types on
  materialization: `wk::wkb()` for WKB, `wk::wkt()` for WKT, and
  `wk::rct()` for BBOX. CRS from the layer's spatial reference is
  attached to all geometry vectors.

* New `sql_translation()` method provides SpatiaLite function translations
  for the SQLITE dialect. R functions like `st_area()`, `st_intersects()`,
  `st_buffer()`, `st_transform()` etc. translate to their SpatiaLite SQL
  equivalents, enabling idiomatic dplyr pipelines with spatial operations:
  `lazysf("countries.gpkg") |> filter(st_area(geom) > 1e6)`.
  OGRSQL dialect falls back to base SQLite translations only.

* Added `sql_escape_logical()` method: TRUE/FALSE map to 1/0 for SQLite,
  NA maps to NULL.

* Added `supports_window_clause()` method (returns TRUE for SQLITE dialect).

* Future-proofed for upcoming dbplyr `sql_dialect()` generic. When the
  next version of dbplyr ships with the new dialect system, lazysf will
  automatically provide SQLite translation without any code changes.

* New `use_arrow` argument to `dbConnect()` and `lazysf()` enables GDAL's
  Arrow C stream interface for reading features. Columnar transfer via
  `GDALVector$getArrowStream()` and nanoarrow, typically much faster for
  larger datasets. Requires GDAL >= 3.6. Configurable globally via
  `options(lazysf.use_arrow = ...)`.

* `st_as_sf()` method is conditionally registered when sf is loaded,
  with automatic geometry conversion from wk types.

## Bug fixes

* Multiline SQL from dbplyr is now collapsed to a single line before passing
  to `GDALVector`. GDAL's constructor rejects SQL with newline characters,
  which dbplyr generates for `rename()`, `transmute()`, and any verb that
  enumerates columns explicitly.

* Table-qualified column prefixes from GDAL (e.g. `nc.AREA` from
  `SELECT "nc".*`) are now stripped automatically so column names match
  what dbplyr expects.

* `OGRFeatureSet` class is stripped from fetch results. The gdalraster print
  method for this class conflicts with wk-typed geometry columns.

* FID column name is normalized to uppercase `"FID"` in both the `fetch()`
  and Arrow paths, matching GDAL's behavior (GeoPackage stores `"fid"` but
  `fetch()` returns `"FID"`).

* Geometry column from `getLayerDefn()` is excluded from the attribute field
  loop in `.schema_from_defn()` — `getLayerDefn()` includes geometry as a
  field but `fetch()` handles it separately via `returnGeomAs`.

## Known issues

* GDAL's GDALVector initialization emits "SpatiaLite is not available"
  warnings on systems without SpatiaLite linked. These are harmless C++
  diagnostics that can't be suppressed from R. Tracked upstream in gdalraster.

* No window functions: `slice_min()`, `slice_max()`, `row_number()` etc.
  are not supported by GDAL's SQLite engine. Use `arrange() |> head()`
  instead.

* Spatial SQL function availability depends on GDAL build configuration.
  `ST_Area()` and `ST_SRID()` work without SpatiaLite (GDAL-native).
  `ST_AsText()`, `ST_Intersects()`, and most geometry operations require
  SpatiaLite.

## Dependencies

* gdalraster (>= 2.0.0) replaces sf in Imports.
* wk added to Imports for geometry type marking.
* sf moves to Suggests (only required for `st_as_sf()`).
* nanoarrow added to Suggests (for Arrow stream interface).
* dbplyr (>= 2.0.0) now required (2nd edition backend API).
* magrittr removed from dependencies.

# lazysf 0.3.0

* Added `dbplyr_edition` method to declare compatibility with the dbplyr 2nd
  edition backend API (#9, thanks @hadley).

* Now requires dbplyr (>= 2.0.0).

# lazysf 0.2.0

* Fixed doc links thanks to CRAN. 

* Moved to hypertidy org. 

# lazysf 0.1.0

* First release.
