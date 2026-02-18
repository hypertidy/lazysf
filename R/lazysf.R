#' Delayed (lazy) read for GDAL vector
#'
#' A lazy data frame for GDAL drawings ('vector data sources'). lazysf is DBI
#' compatible and designed to work with dplyr. It should work with any data source
#' (file, url, connection string) readable by GDAL via the gdalraster package.
#'
#' Lazy means that the usual behaviour of reading the entirety of a data source
#' into memory is avoided. Printing the output results in a preview query being
#' run and displayed (the top few rows of data).
#'
#' The output of `lazysf()` is a 'tbl_GDALVectorConnection` that extends `tbl_dbi` and
#' may be used with functions and workflows in the normal DBI way, see [GDALSQL()] for
#' the lazysf DBI support.
#'
#' The kind of query that may be run will depend on the type of format, see the
#' list on the GDAL vector drivers page. For some details see the
#' [GDALSQL vignette](https://hypertidy.github.io/lazysf/articles/GDALSQL.html).
#'
#' When dplyr is attached the lazy data frame can be used with the usual
#' verbs (filter, select, distinct, mutate, transmute, arrange, left_join, pull,
#' collect etc.). To see the result as a SQL query rather than a data frame
#' preview use `dplyr::show_query()`.
#'
#' To obtain an in memory data frame use an explicit `collect()`.
#' If the sf package is installed, `st_as_sf()` will collect and convert to an
#' sf data frame. A result may not contain a geometry column, in which case
#' `st_as_sf()` will fail.
#'
#' As well as `collect()` it's also possible to use `tibble::as_tibble()` or
#' `as.data.frame()` or `pull()` which all force computation and retrieve the
#' result.
#'
#' @param x the data source name (file path, url, or database connection string
#'   - analogous to a GDAL dsn) or a `GDALVectorConnection`
#' @param layer layer name; defaults to the first layer
#' @param ... ignored
#' @param query SQL query to pass in directly
#' @return a 'tbl_GDALVectorConnection', extending 'tbl_lazy' (something that works
#'   with dplyr verbs, and only shows a preview until you commit the result via
#'   [collect()]) see Details
#' @export
#'
#' @examples
#' ## a multi-layer file
#' f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
#' lazysf(f)
#'
#' \donttest{
#' ## Geopackage (an actual database, so with SELECT we must be explicit re geom-column)
#' nc <- system.file("extdata/nc.gpkg", package = "lazysf", mustWork = TRUE)
#' lazysf(nc)
#' lazysf(nc, query = "SELECT AREA, FIPS, geom FROM nc WHERE AREA < 0.1")
#' lazysf(nc, layer = "nc") |> dplyr::select(AREA, FIPS, geom) |> dplyr::filter(AREA < 0.1)
#'
#' ## the famous ESRI Shapefile (not an actual database)
#' shdb <- system.file("extdata/nc.shp", package = "lazysf", mustWork = TRUE)
#' shp <- lazysf(shdb)
#' library(dplyr)
#' shp |>
#'  filter(NAME %LIKE% 'A%') |>
#'  mutate(abc = 1.3) |>
#'  select(abc, NAME, `_ogr_geometry_`) |>
#'  arrange(desc(NAME))
#' }
lazysf <- function(x, layer, ...) {
  UseMethod("lazysf")
}
#' @param geom_format geometry output format, passed to [dbConnect()]
#' @param dialect SQL dialect, passed to [dbConnect()]
#' @name lazysf
#' @export
lazysf.character <- function(x, layer, ..., query = NA,
                             geom_format = getOption("lazysf.geom_format", "WKB"),
                             dialect = getOption("lazysf.dialect", "SQLITE")) {
  db <- dbConnect(GDALSQL(), x, geom_format = geom_format, dialect = dialect)
  lazysf(db, layer, ..., query = query)
}
#' @name lazysf
#' @export
lazysf.GDALVectorConnection <- function(x, layer, ..., query = NA) {
  if (!is.na(query)) {
    if (!missing(layer)) message("'layer' argument ignored, using 'query'")
    return(dplyr::tbl(x, dbplyr::sql(query)))
  }
  if (missing(layer) || is.numeric(layer)) {
    layers <- dbListTables(x)
    layer <- layers[1L]
  }
  dplyr::tbl(x, layer)
}


#' Force computation of a GDAL query
#'
#' Convert lazysf to an in memory data frame or sf object
#'
#' `collect()` retrieves data into a local table, preserving grouping and ordering.
#'
#' `st_as_sf()` retrieves data into a local sf data frame. Requires the sf
#' package to be installed, and will succeed only if the result contains a
#' geometry column (WKB or WKT via `geom_format`). The method is registered
#' when sf is loaded.
#'
#' @param x output of [lazysf()]
#' @param ... passed to [collect()]
#' @name collect
#' @return a data frame from `collect()`, sf data frame from `st_as_sf()`
#'   (only if it contains geometry)
#' @seealso lazysf
#' @importFrom dplyr collect
#' @export collect
#' @examples
#' f <- system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
#' lsf <- lazysf(f)
#' dplyr::collect(lsf)
#'
"collect"

## registered conditionally in .onLoad when sf is available
st_as_sf.tbl_GDALVectorConnection <- function(x, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to convert to sf. ",
         "Install with: install.packages('sf')", call. = FALSE)
  }
  d <- dplyr::collect(x, ...)
  ## find geometry columns by wk type (wk::wkb, wk::wkt, or wk::rct)
  is_geom <- vapply(d, function(col) {
    inherits(col, "wk_wkb") || inherits(col, "wk_wkt") || inherits(col, "wk_rct")
  }, logical(1))
  if (!any(is_geom)) {
    stop("No geometry column found in result. ",
         "Is geom_format set to 'NONE'?", call. = FALSE)
  }
  geom_col <- names(which(is_geom))[1L]
  d[[geom_col]] <- sf::st_as_sfc(d[[geom_col]])
  sf::st_as_sf(d)
}

