# Delayed (lazy) read for GDAL vector

A lazy data frame for GDAL drawings ('vector data sources'). lazysf is
DBI compatible and designed to work with dplyr. It should work with any
data source (file, url, connection string) readable by GDAL via the
gdalraster package.

## Usage

``` r
lazysf(x, layer, ...)

# S3 method for class 'character'
lazysf(
  x,
  layer,
  ...,
  query = NA,
  geom_format = getOption("lazysf.geom_format", "WKB"),
  dialect = getOption("lazysf.dialect", "SQLITE"),
  use_arrow = getOption("lazysf.use_arrow", FALSE)
)

# S3 method for class 'GDALVectorConnection'
lazysf(x, layer, ..., query = NA)
```

## Arguments

- x:

  the data source name (file path, url, or database connection string

  - analogous to a GDAL dsn) or a `GDALVectorConnection`

- layer:

  layer name; defaults to the first layer

- ...:

  ignored

- query:

  SQL query to pass in directly

- geom_format:

  geometry output format, passed to
  [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- dialect:

  SQL dialect, passed to
  [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- use_arrow:

  logical; if `TRUE`, use GDAL's Arrow C stream interface for reading
  features. Passed to
  [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

## Value

a 'tbl_GDALVectorConnection', extending 'tbl_lazy' (something that works
with dplyr verbs, and only shows a preview until you commit the result
via
[`collect()`](https://hypertidy.github.io/lazysf/reference/collect.md))
see Details

## Details

Lazy means that the usual behaviour of reading the entirety of a data
source into memory is avoided. Printing the output results in a preview
query being run and displayed (the top few rows of data).

The output of `lazysf()` is a
'tbl_GDALVectorConnection`that extends`tbl_dbi\` and may be used with
functions and workflows in the normal DBI way, see
[`GDALSQL()`](https://hypertidy.github.io/lazysf/reference/GDALSQL.md)
for the lazysf DBI support.

The kind of query that may be run will depend on the type of format, see
the list on the GDAL vector drivers page. For some details see the
[GDALSQL
vignette](https://hypertidy.github.io/lazysf/articles/GDALSQL.html).

When dplyr is attached the lazy data frame can be used with the usual
verbs (filter, select, distinct, mutate, transmute, arrange, left_join,
pull, collect etc.). To see the result as a SQL query rather than a data
frame preview use
[`dplyr::show_query()`](https://dplyr.tidyverse.org/reference/explain.html).

To obtain an in memory data frame use an explicit
[`collect()`](https://hypertidy.github.io/lazysf/reference/collect.md).
Geometry columns in the result are wk-typed vectors
([`wk::wkb`](https://paleolimbot.github.io/wk/reference/wkb.html),
[`wk::wkt`](https://paleolimbot.github.io/wk/reference/wkt.html), or
[`wk::rct`](https://paleolimbot.github.io/wk/reference/rct.html)) with
CRS attached. To convert to an sf data frame, collect first then call
`sf::st_as_sf()`: `lazysf(dsn) |> collect() |> sf::st_as_sf()`.

As well as
[`collect()`](https://hypertidy.github.io/lazysf/reference/collect.md)
it's also possible to use
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
or [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or
[`pull()`](https://dplyr.tidyverse.org/reference/pull.html) which all
force computation and retrieve the result.

## Examples

``` r
## a multi-layer file
f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
lazysf(f)
#> # A query:  ?? x 3
#> # Database: GDAL <SQLITE> WKB [/home/runner/work/_temp/Library/lazysf/extdata/...]
#>       FID NAME                         geom                                     
#>   <int64> <chr>                        <wk_wkb>                                 
#> 1       1 New South Wales              <MULTIPOLYGON (((150.7016 -35.12286, 150…
#> 2       2 Victoria                     <MULTIPOLYGON (((146.6196 -38.70196, 146…
#> 3       3 Queensland                   <MULTIPOLYGON (((148.8473 -20.3457, 148.…
#> 4       4 South Australia              <MULTIPOLYGON (((137.3481 -34.48242, 137…
#> 5       5 Western Australia            <MULTIPOLYGON (((126.3868 -14.01168, 126…
#> 6       6 Tasmania                     <MULTIPOLYGON (((147.8397 -40.29844, 147…
#> 7       7 Northern Territory           <MULTIPOLYGON (((136.3669 -13.84237, 136…
#> 8       8 Australian Capital Territory <MULTIPOLYGON (((149.2317 -35.222, 149.2…
#> 9       9 Other Territories            <MULTIPOLYGON (((167.9333 -29.05421, 167…

# \donttest{
## Geopackage (an actual database, so with SELECT we must be explicit re geom-column)
nc <- system.file("extdata/nc.gpkg", package = "lazysf", mustWork = TRUE)
lazysf(nc)
#> # A query:  ?? x 16
#> # Database: GDAL <SQLITE> WKB [/home/runner/work/_temp/Library/lazysf/extdata/...]
#>        FID  AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74
#>    <int64> <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>
#>  1       1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1
#>  2       2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0
#>  3       3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5
#>  4       4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1
#>  5       5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9
#>  6       6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7
#>  7       7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0
#>  8       8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0
#>  9       9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4
#> 10      10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1
#> # ℹ more rows
#> # ℹ 5 more variables: NWBIR74 <dbl>, BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geom <wk_wkb>
lazysf(nc, query = "SELECT AREA, FIPS, geom FROM nc WHERE AREA < 0.1")
#> # A query:  ?? x 4
#> # Database: GDAL <SQLITE> WKB [/home/runner/work/_temp/Library/lazysf/extdata/...]
#>        FID  AREA FIPS  geom                                                     
#>    <int64> <dbl> <chr> <wk_wkb>                                                 
#>  1       0 0.061 37005 <MULTIPOLYGON (((-81.23989 36.36536, -81.24069 36.37942,…
#>  2       1 0.07  37053 <MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36.33773, …
#>  3       2 0.097 37091 <MULTIPOLYGON (((-76.74506 36.23392, -76.98069 36.23024,…
#>  4       3 0.062 37029 <MULTIPOLYGON (((-76.00897 36.3196, -75.95718 36.19377, …
#>  5       4 0.091 37073 <MULTIPOLYGON (((-76.56251 36.34057, -76.60424 36.31498,…
#>  6       5 0.072 37181 <MULTIPOLYGON (((-78.49252 36.17359, -78.51472 36.17522,…
#>  7       6 0.053 37139 <MULTIPOLYGON (((-76.29893 36.21423, -76.32423 36.23362,…
#>  8       7 0.081 37189 <MULTIPOLYGON (((-81.80622 36.10456, -81.81715 36.10939,…
#>  9       8 0.063 37143 <MULTIPOLYGON (((-76.48053 36.07979, -76.53696 36.08792,…
#> 10       9 0.044 37041 <MULTIPOLYGON (((-76.68874 36.29452, -76.64822 36.31532,…
#> # ℹ more rows
lazysf(nc, layer = "nc") |> dplyr::select(AREA, FIPS, geom) |> dplyr::filter(AREA < 0.1)
#> # A query:  ?? x 3
#> # Database: GDAL <SQLITE> WKB [/home/runner/work/_temp/Library/lazysf/extdata/...]
#>        FID  AREA FIPS  geom                                                     
#>    <int64> <dbl> <chr> <wk_wkb>                                                 
#>  1       0 0.061 37005 <MULTIPOLYGON (((-81.23989 36.36536, -81.24069 36.37942,…
#>  2       1 0.07  37053 <MULTIPOLYGON (((-76.00897 36.3196, -76.01735 36.33773, …
#>  3       2 0.097 37091 <MULTIPOLYGON (((-76.74506 36.23392, -76.98069 36.23024,…
#>  4       3 0.062 37029 <MULTIPOLYGON (((-76.00897 36.3196, -75.95718 36.19377, …
#>  5       4 0.091 37073 <MULTIPOLYGON (((-76.56251 36.34057, -76.60424 36.31498,…
#>  6       5 0.072 37181 <MULTIPOLYGON (((-78.49252 36.17359, -78.51472 36.17522,…
#>  7       6 0.053 37139 <MULTIPOLYGON (((-76.29893 36.21423, -76.32423 36.23362,…
#>  8       7 0.081 37189 <MULTIPOLYGON (((-81.80622 36.10456, -81.81715 36.10939,…
#>  9       8 0.063 37143 <MULTIPOLYGON (((-76.48053 36.07979, -76.53696 36.08792,…
#> 10       9 0.044 37041 <MULTIPOLYGON (((-76.68874 36.29452, -76.64822 36.31532,…
#> # ℹ more rows

## the famous ESRI Shapefile (not an actual database)
shdb <- system.file("extdata/nc.shp", package = "lazysf", mustWork = TRUE)
shp <- lazysf(shdb)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
shp |>
 filter(NAME %LIKE% 'A%') |>
 mutate(abc = 1.3) |>
 select(abc, NAME) |>
 arrange(desc(NAME))
#> # A query:    ?? x 2
#> # Database:   GDAL <SQLITE> WKB [/home/runner/work/_temp/Library/lazysf/extdata/...]
#> # Ordered by: desc(NAME)
#>       FID   abc NAME     
#>   <int64> <dbl> <chr>    
#> 1       0   1.3 Avery    
#> 2       1   1.3 Ashe     
#> 3       2   1.3 Anson    
#> 4       3   1.3 Alleghany
#> 5       4   1.3 Alexander
#> 6       5   1.3 Alamance 
# }
```
