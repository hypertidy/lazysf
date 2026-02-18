
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazysf

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/lazysf)](https://CRAN.R-project.org/package=lazysf)
[![](http://cranlogs.r-pkg.org/badges/last-month/lazysf?color=green)](https://cran.r-project.org/package=lazysf)
<!-- badges: end -->

lazysf provides a dplyr backend for any vector data source readable by
GDAL. It uses `gdalraster::GDALVector` to talk to GDAL and `dbplyr` for
SQL translation, giving you lazy evaluation of spatial data through
familiar dplyr verbs.

Vector data sources — files, URLs, databases, cloud storage — are
accessed through a DBI connection. Nothing is read into memory until you
call `collect()`.

## Quick start

``` r
library(lazysf)
library(dplyr)

f <- system.file("extdata/nc.gpkg", package = "lazysf", mustWork = TRUE)
lf <- lazysf(f)
lf
#> Warning in new_CppObject_xp(fields$.module, fields$.pointer, ...): SpatiaLite
#> is not available
#> # Source:   table<"nc"> [?? x 16]
#> # Database: GDAL <SQLITE> WKB [/perm_storage/home/mdsumner/R/x86_64-pc-linux-g...]
#>        FID  AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74
#>    <int64> <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>
#>  1       1 0.114     1.442  1825    1825 Ashe  37009  37009        5  1091     1
#>  2       2 0.061     1.231  1827    1827 Alle… 37005  37005        3   487     0
#>  3       3 0.143     1.63   1828    1828 Surry 37171  37171       86  3188     5
#>  4       4 0.07      2.968  1831    1831 Curr… 37053  37053       27   508     1
#>  5       5 0.153     2.206  1832    1832 Nort… 37131  37131       66  1421     9
#>  6       6 0.097     1.67   1833    1833 Hert… 37091  37091       46  1452     7
#>  7       7 0.062     1.547  1834    1834 Camd… 37029  37029       15   286     0
#>  8       8 0.091     1.284  1835    1835 Gates 37073  37073       37   420     0
#>  9       9 0.118     1.421  1836    1836 Warr… 37185  37185       93   968     4
#> 10      10 0.124     1.428  1837    1837 Stok… 37169  37169       85  1612     1
#> # ℹ more rows
#> # ℹ 5 more variables: NWBIR74 <dbl>, BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geom <wk_wkb>
```

Standard dplyr verbs generate SQL that GDAL executes:

``` r
lf |>
  filter(AREA < 0.1) |>
  select(NAME, AREA, geom) |>
  arrange(AREA)
#> Warning in new_CppObject_xp(fields$.module, fields$.pointer, ...): SpatiaLite
#> is not available
#> # Source:     SQL [?? x 3]
#> # Database:   GDAL <SQLITE> WKB [/perm_storage/home/mdsumner/R/x86_64-pc-linux-g...]
#> # Ordered by: AREA
#>        FID NAME         AREA geom                                               
#>    <int64> <chr>       <dbl> <wk_wkb>                                           
#>  1       0 New Hanover 0.042 <MULTIPOLYGON (((-77.96073 34.18924, -77.96587 34.…
#>  2       1 Chowan      0.044 <MULTIPOLYGON (((-76.68874 36.29452, -76.64822 36.…
#>  3       2 Clay        0.051 <MULTIPOLYGON (((-83.938 34.98939, -83.98855 34.98…
#>  4       3 Pasquotank  0.053 <MULTIPOLYGON (((-76.29893 36.21423, -76.32423 36.…
#>  5       4 Mitchell    0.059 <MULTIPOLYGON (((-82.11885 35.81853, -82.14665 35.…
#>  6       5 Polk        0.06  <MULTIPOLYGON (((-82.21017 35.19313, -82.27833 35.…
#>  7       6 Alleghany   0.061 <MULTIPOLYGON (((-81.23989 36.36536, -81.24069 36.…
#>  8       7 Camden      0.062 <MULTIPOLYGON (((-76.00897 36.3196, -75.95718 36.1…
#>  9       8 Perquimans  0.063 <MULTIPOLYGON (((-76.48053 36.07979, -76.53696 36.…
#> 10       9 Avery       0.064 <MULTIPOLYGON (((-81.94135 35.95498, -81.9614 35.9…
#> # ℹ more rows
```

Use `show_query()` to see the SQL:

``` r
lf |>
  filter(AREA < 0.1) |>
  select(NAME, AREA, geom) |>
  arrange(AREA) |>
  show_query()
#> <SQL>
#> SELECT "NAME", "AREA", "geom"
#> FROM "nc"
#> WHERE ("AREA" < 0.1)
#> ORDER BY "AREA"
```

Use `collect()` to pull data into memory:

``` r
d <- lf |>
  filter(AREA < 0.1) |>
  select(NAME, AREA, geom) |>
  collect()
#> Warning in new_CppObject_xp(fields$.module, fields$.pointer, ...): SpatiaLite
#> is not available
d
#> # A tibble: 34 × 4
#>        FID NAME        AREA geom                                                
#>    <int64> <chr>      <dbl> <wk_wkb>                                            
#>  1       0 Alleghany  0.061 <MULTIPOLYGON (((-81.23989 36.36536, -81.24069 36.3…
#>  2       1 Currituck  0.07  <MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36.33…
#>  3       2 Hertford   0.097 <MULTIPOLYGON (((-76.74506 36.23392, -76.98069 36.2…
#>  4       3 Camden     0.062 <MULTIPOLYGON (((-76.00897 36.3196, -75.95718 36.19…
#>  5       4 Gates      0.091 <MULTIPOLYGON (((-76.56251 36.34057, -76.60424 36.3…
#>  6       5 Vance      0.072 <MULTIPOLYGON (((-78.49252 36.17359, -78.51472 36.1…
#>  7       6 Pasquotank 0.053 <MULTIPOLYGON (((-76.29893 36.21423, -76.32423 36.2…
#>  8       7 Watauga    0.081 <MULTIPOLYGON (((-81.80622 36.10456, -81.81715 36.1…
#>  9       8 Perquimans 0.063 <MULTIPOLYGON (((-76.48053 36.07979, -76.53696 36.0…
#> 10       9 Chowan     0.044 <MULTIPOLYGON (((-76.68874 36.29452, -76.64822 36.3…
#> # ℹ 24 more rows
```

## Spatial SQL (SQLITE dialect)

lazysf defaults to the SQLITE dialect, which gives you access to spatial
SQL functions. These are translated from R-style names to their SQL
equivalents:

``` r
lf |>
  mutate(area_m2 = st_area(geom)) |>
  filter(st_area(geom) > 0.1) |>
  mutate(srid = st_srid(geom)) |>
  collect()
```

Functions like `st_area()`, `st_srid()`, and `st_geometrytype()` work
via GDAL’s built-in SQLite engine (no SpatiaLite extension needed).
Functions that operate on geometry values (like `st_astext()`,
`st_intersects()`, `st_buffer()`) require a SpatiaLite-enabled GDAL
build.

## Arrow stream interface

For larger datasets, enable GDAL’s columnar Arrow C stream interface for
faster data transfer:

``` r
lf <- lazysf(f, use_arrow = TRUE)
lf |> collect()
```

This uses `GDALVector$getArrowStream()` via nanoarrow — data moves from
GDAL to R in columnar batches instead of row-by-row. Requires GDAL \>=
3.6.

## SQL dialects

lazysf supports two SQL dialects:

**SQLITE** (default): Full SQLite syntax including subqueries, spatial
functions, `CAST`, `GROUP BY`, `ORDER BY`. Required for dbplyr to work
properly. This is GDAL’s embedded SQLite engine, available for any
format.

``` r
lazysf(f) |>
  group_by(SID74) |>
  summarise(n = n(), mean_area = mean(AREA, na.rm = TRUE)) |>
  collect()
#> Warning in new_CppObject_xp(fields$.module, fields$.pointer, ...): SpatiaLite
#> is not available
#> # A tibble: 23 × 4
#>        FID SID74     n mean_area
#>    <int64> <dbl> <int>     <dbl>
#>  1       0     0    13  0.084846
#>  2       1     1    11  0.083091
#>  3       2     2     8  0.13088 
#>  4       3     3     6  0.10783 
#>  5       4     4    13  0.14715 
#>  6       5     5    11  0.12545 
#>  7       6     6     4  0.16575 
#>  8       7     7     4  0.14975 
#>  9       8     8     5  0.134   
#> 10       9     9     2  0.1605  
#> # ℹ 13 more rows
```

**OGRSQL**: GDAL’s native OGR SQL. Simpler, no subquery support, but
works for basic operations:

``` r
lazysf(f, dialect = "OGRSQL") |>
  filter(NAME %LIKE% "A%") |>
  collect()
```

## DBI connection

For more control, use the DBI interface directly:

``` r
con <- dbConnect(GDALSQL(), f)
con
#> <GDALVectorConnection>
#>       DSN: /perm_storage/home/mdsumner/R/x86_64-pc-linux-gnu-library/4.5/lazysf/extdata/nc.gpkg
#>   dialect: SQLITE
#>  geometry: WKB
#>     arrow: off
#>    tables: nc
dbListTables(con)
#> [1] "nc"
dbListFields(con, "nc")
#>  [1] "AREA"      "PERIMETER" "CNTY_"     "CNTY_ID"   "NAME"      "FIPS"     
#>  [7] "FIPSNO"    "CRESS_ID"  "BIR74"     "SID74"     "NWBIR74"   "BIR79"    
#> [13] "SID79"     "NWBIR79"   "geom"
DBI::dbDisconnect(con)
#> <GDALVectorConnection>
#>       DSN: 
#>   dialect: SQLITE
#>  geometry: WKB
#>     arrow: off
#>    tables: (unavailable)
```

## Geometry formats

lazysf supports four geometry output formats, set via `geom_format`:

- `"WKB"` (default): Well-Known Binary, as `wk::wkb` vectors
- `"WKT"`: Well-Known Text, as `wk::wkt` vectors
- `"BBOX"`: Bounding box per feature, as `wk::rct` vectors
- `"NONE"`: No geometry (attributes only, faster for non-spatial
  queries)

``` r
lazysf(f, geom_format = "WKT") |>
  select(NAME, geom) |>
  head(3) |>
  collect()
#> Warning in new_CppObject_xp(fields$.module, fields$.pointer, ...): SpatiaLite
#> is not available
#> # A tibble: 3 × 3
#>       FID NAME      geom                                                        
#>   <int64> <chr>     <wk_wkt>                                                    
#> 1       0 Ashe      MULTIPOLYGON (((-81.47276 36.23436, -81.54084 36.27251, -81…
#> 2       1 Alleghany MULTIPOLYGON (((-81.23989 36.36536, -81.24069 36.37942, -81…
#> 3       2 Surry     MULTIPOLYGON (((-80.45634 36.24256, -80.47639 36.25473, -80…
```

## Online sources

Any GDAL-readable source works — local files, URLs, cloud storage,
databases:

``` r
## GeoJSON from URL
lazysf("https://example.com/data.geojson")

## PostgreSQL/PostGIS
lazysf("PG:host=localhost dbname=mydb user=me password=secret")

## Cloud-optimized sources via /vsicurl/
lazysf("/vsicurl/https://example.com/large.gpkg")
```

## Installation

``` r
# From CRAN
install.packages("lazysf")

# Development version from r-universe
options(repos = c(
  hypertidy = "https://hypertidy.r-universe.dev",
  CRAN = "https://cloud.r-project.org"))
install.packages("lazysf")
```

## Known limitations

- **No window functions**: `slice_min()`, `slice_max()`, `row_number()`
  etc. are not supported by GDAL’s SQLite engine.
- **OGRSQL dialect**: Does not support subqueries, so complex dplyr verb
  chains may fail. Use SQLITE dialect (the default) instead.
- **FID is sticky**: GDAL always includes the feature ID column in
  results, even when your SQL doesn’t select it.
- **Format-dependent**: Performance and capability varies by GDAL
  driver. File formats like Shapefile and GeoPackage work well; text
  formats like CSV or GeoJSON are slower for large data.

------------------------------------------------------------------------

## Code of Conduct

Please note that the lazysf project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
