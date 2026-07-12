# GDAL SQL with lazysf

``` r

library(lazysf)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Overview

lazysf translates dplyr verbs into SQL and executes them via GDAL. The
SQL dialect determines what’s possible — and lazysf defaults to SQLITE,
which is the most capable option.

GDAL provides two SQL dialects for querying vector data:

1.  **SQLITE**: A full SQLite engine embedded in GDAL. Supports
    subqueries, `GROUP BY`, `ORDER BY`, `CAST`, and spatial SQL
    functions. Available for *any* format — GDAL runs SQLite in-process
    even for Shapefiles.
2.  **OGRSQL**: GDAL’s native SQL engine. Simpler, format-specific
    features like `%LIKE%`, but no subquery support.

## SQLITE dialect (default)

The SQLITE dialect is what makes lazysf work well with dbplyr. Most
dplyr verbs generate subqueries, and only SQLITE handles those
correctly.

``` r

f <- system.file("extdata/nc.gpkg", package = "lazysf", mustWork = TRUE)
lf <- lazysf(f)

## filter + select + arrange
lf |>
  filter(AREA < 0.1) |>
  select(NAME, AREA, geom) |>
  arrange(AREA) |>
  collect()
#> # A tibble: 34 × 4
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
#> # ℹ 24 more rows
```

Aggregation works:

``` r

lf |>
  group_by(SID74) |>
  summarise(n = n(), mean_area = mean(AREA, na.rm = TRUE)) |>
  arrange(desc(n)) |>
  head(5) |>
  collect()
#> # A tibble: 5 × 4
#>       FID SID74     n mean_area
#>   <int64> <dbl> <int>     <dbl>
#> 1       0     4    13    0.147 
#> 2       1     0    13    0.0848
#> 3       2     5    11    0.125 
#> 4       3     1    11    0.0831
#> 5       4     2     8    0.131
```

## Spatial SQL functions

lazysf provides SQL translations for spatial functions. These translate
R-style `st_*()` names to their SQL equivalents (e.g. `st_area()` →
`ST_Area()`).

### GDAL-native functions (no SpatiaLite needed)

These work on any system with GDAL’s SQLITE dialect:

``` r

## Measurements and accessors
lf |>
  mutate(
    area = st_area(geom),
    srid = st_srid(geom)
  ) |>
  head(5) |>
  collect()
```

Functions available without SpatiaLite include `st_area()`, `st_srid()`,
`st_minx()`, `st_maxx()`, `st_miny()`, `st_maxy()`.

### SpatiaLite functions (need SpatiaLite-enabled GDAL)

With SpatiaLite linked, the full set of spatial SQL becomes available:

``` r

## Spatial predicates
lf |>
  filter(st_intersects(geom, st_geomfromtext("POLYGON((-80 35, -79 35, -79 36, -80 36, -80 35))"))) |>
  collect()

## Geometry operations
lf |>
  mutate(
    wkt = st_astext(geom),
    centroid = st_centroid(geom),
    buffered = st_buffer(geom, 0.01)
  ) |>
  collect()

## Coordinate transformation
lf |>
  mutate(geom_3857 = st_transform(geom, 3857)) |>
  collect()
```

To check if SpatiaLite is available in your GDAL build:

``` r

con <- dbConnect(GDALSQL(), f)
tryCatch(
  DBI::dbGetQuery(con, "SELECT spatialite_version()"),
  error = function(e) message("SpatiaLite not available")
)
```

## OGRSQL dialect

OGRSQL is simpler but has format-specific features. Set it explicitly:

``` r

lf_ogr <- lazysf(f, dialect = "OGRSQL")

## Basic filter/select work
lf_ogr |>
  filter(AREA < 0.1) |>
  select(NAME, AREA) |>
  collect()
```

OGRSQL limitations with dbplyr:

- **No subqueries**: Operations like
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html),
  or chained verbs that dbplyr wraps in subqueries will fail.
- **No `GROUP BY` with subqueries**: Simple aggregations may work,
  complex ones won’t.

Use OGRSQL when you need format-specific features or are working with a
data source where SQLITE dialect isn’t available.

## Direct SQL

You can pass raw SQL directly:

``` r

lazysf(f, query = "SELECT NAME, AREA, geom FROM nc WHERE AREA < 0.1 ORDER BY AREA") |>
  collect()
#> # A tibble: 34 × 4
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
#> # ℹ 24 more rows
```

## GDAL quirks

A few things to know when working with GDAL’s SQL engine:

**FID is sticky.** GDAL always includes the feature ID in results, like
sf’s sticky geometry column. Your SQL might select two columns but
you’ll get three back.

**Table-qualified wildcards.** When dbplyr generates `SELECT "nc".*`,
GDAL returns column names prefixed with the table name (`nc.AREA`,
`nc.NAME`). lazysf strips these prefixes automatically.

**No window functions.** `RANK()`, `ROW_NUMBER()`, `DENSE_RANK()` etc.
are not available. This means
[`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html),
[`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html), and
[`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html) won’t
work. Use `arrange() |> head()` instead.

``` r

## This won't work:
lf |> slice_min(AREA, n = 5)

## Do this instead:
lf |> arrange(AREA) |> head(5) |> collect()
```

## Arrow stream interface

For large datasets, the Arrow C stream interface provides columnar data
transfer from GDAL, which can be significantly faster than the default
row-based `fetch()`:

``` r

lf <- lazysf(f, use_arrow = TRUE)
d <- lf |> collect()
```

This uses `GDALVector$getArrowStream()` via nanoarrow. Data moves from
GDAL to R in batches of up to 65,536 features without going through the
OGRFeature abstraction. Requires GDAL \>= 3.6 and the nanoarrow package.

The Arrow path produces identical results to the default path — same
column names, same geometry handling, same wk type marking.

## Geometry formats

lazysf can return geometry in several formats:

``` r

## WKB (default) — compact binary, interoperable with sf and wk
lazysf(f, geom_format = "WKB")

## WKT — human-readable text
lazysf(f, geom_format = "WKT")

## BBOX — bounding box per feature (fast, no full geometry)
lazysf(f, geom_format = "BBOX")

## NONE — no geometry at all (fastest for attribute-only queries)
lazysf(f, geom_format = "NONE")
```

All geometry columns are marked with wk types
([`wk::wkb`](https://paleolimbot.github.io/wk/reference/wkb.html),
[`wk::wkt`](https://paleolimbot.github.io/wk/reference/wkt.html),
[`wk::rct`](https://paleolimbot.github.io/wk/reference/rct.html)) so
they interoperate with the wk ecosystem. To convert to an sf data frame,
collect first then call `sf::st_as_sf()`:

``` r

lazysf(f) |> collect() |> sf::st_as_sf()
```

## Global options

lazysf respects these options for defaults:

- `lazysf.dialect`: default SQL dialect (default: `"SQLITE"`)
- `lazysf.geom_format`: default geometry format (default: `"WKB"`)
- `lazysf.use_arrow`: default Arrow stream setting (default: `FALSE`)
- `lazysf.query.debug`: if `TRUE`, print SQL and row counts for every
  query

``` r

options(lazysf.query.debug = TRUE)
lazysf(f) |> filter(AREA < 0.1) |> collect()
## prints: SQL and row counts for each query dbplyr issues
```
