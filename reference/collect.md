# Collect a lazy GDAL query into memory

Re-export of
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).
Forces evaluation of a lazy `tbl_GDALVectorConnection` and returns the
result as a local tibble. Geometry columns are returned as wk-typed
vectors
([`wk::wkb`](https://paleolimbot.github.io/wk/reference/wkb.html),
[`wk::wkt`](https://paleolimbot.github.io/wk/reference/wkt.html), or
[`wk::rct`](https://paleolimbot.github.io/wk/reference/rct.html)) with
CRS attached, and can be passed to `sf::st_as_sf()` for conversion.

## Arguments

- x:

  A `tbl_GDALVectorConnection` created by
  [`lazysf()`](https://hypertidy.github.io/lazysf/reference/lazysf.md).

- ...:

  Additional arguments passed to
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

A local tibble.

## See also

[`lazysf()`](https://hypertidy.github.io/lazysf/reference/lazysf.md),
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)

## Examples

``` r
f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
lsf <- lazysf(f)
dplyr::collect(lsf)
#> # A tibble: 9 × 3
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
```
