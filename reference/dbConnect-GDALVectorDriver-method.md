# dbConnect

dbConnect for vector data sources readable by GDAL

## Usage

``` r
# S4 method for class 'GDALVectorDriver'
dbConnect(
  drv,
  DSN = "",
  readonly = TRUE,
  geom_format = getOption("lazysf.geom_format", "WKB"),
  dialect = getOption("lazysf.dialect", "SQLITE"),
  use_arrow = getOption("lazysf.use_arrow", FALSE),
  ...
)
```

## Arguments

- drv:

  GDALVectorDriver created by
  [`GDALSQL()`](https://hypertidy.github.io/lazysf/reference/GDALSQL.md)

- DSN:

  data source name, may be a file, or folder path, database connection
  string, or URL

- readonly:

  open in readonly mode (`TRUE` is the only option currently)

- geom_format:

  geometry output format: `"WKB"` (default), `"WKT"`, `"NONE"`, or
  `"BBOX"` (alias `"RCT"`). Case-insensitive.

- dialect:

  SQL dialect: `"SQLITE"` (default), `"OGRSQL"`, `"INDIRECT_SQLITE"`, or
  `""` (let GDAL choose). SQLITE is recommended as it supports
  subqueries (required for dbplyr) and spatial SQL functions.

- use_arrow:

  logical; if `TRUE`, use GDAL's Arrow C stream interface for reading
  features (GDAL \>= 3.6). Columnar transfer via nanoarrow, typically
  much faster for larger datasets. Default `FALSE`.

- ...:

  ignored

## Details

The 'OGRSQL' available is documented with GDAL:
<https://gdal.org/en/stable/user/ogr_sql_sqlite_dialect.html>

## Examples

``` r
f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
db <- dbConnect(GDALSQL(), f)
dbListTables(db)
#> [1] "state"  "centre"
```
