# lazysf (dev)

## Breaking changes

* The backend now uses gdalraster instead of sf for all GDAL access. sf moves
  from Imports to Suggests and is only needed for `st_as_sf()`.

* DBI class names renamed: `SFSQLConnection` → `GDALVectorConnection`,
  `SFSQLDriver` → `GDALVectorDriver`, `SFSQLResult` → `GDALVectorResult`.
  The driver constructor is now `GDALSQL()` (was `SFSQL()`).

* The magrittr pipe (`%>%`) is no longer re-exported. Use R's native pipe (`|>`)
  or load magrittr yourself.

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

* Added `db_connection_describe()` method for better printing in dbplyr.

* Field/type discovery now uses direct layer access (`$fetch(0)`) instead of
  rewriting SQL. This eliminates the `WHERE (0 = 1)` → `LIMIT 0` hack and
  works reliably with all SQL dialects.

* New `dbListFields()` method uses `$getLayerDefn()` for schema introspection
  without executing SQL.

* `st_as_sf()` method is now conditionally registered when sf is loaded,
  with automatic WKB-to-sfc geometry conversion.

## Dependencies

* gdalraster (>= 2.0.0) replaces sf in Imports.
* sf moves to Suggests (only required for `st_as_sf()`).
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
