#' Delayed (lazy) read for GDAL vector
#'
#' A lazy data frame for GDAL drawings ('vector data sources'). lazysf is DBI
#' compatible and designed to work with dplyr. It should work with any data source
#' (file, url, connection string) readable by the sf package function `sf_read`.
#'
#' Lazy means that the usual behaviour of reading the entirety of a data source
#' into memory is avoided. Printing the output results in a preview query being
#' run and displayed (the top few rows of data).
#'
#' The output of `lazysf()` is a 'tbl_SFSQLConnection` that extends `tbl_dbi` and
#' may be used with functions and workflows in the normal DBI way, see [SFSQL()] for
#' the lazysf DBI support.
#'
#' The kind of q uery that may be run will depend on the type of format, see the
#' list on the GDAL vector drivers page. For some details see the
#' [GDALSQL vignette](https://hypertidy.github.io/lazysf/articles/GDALSQL.html).
#'
#' When dplyr is attached the lazy data frame can be used with the usual verbs
#' verbs (filter, select, distinct, mutate, transmute, arrange, left_join, pull,
#' collect etc.). To see the result as a SQL query rather than a data frame
#' preview use `dplyr::show_query()`.
#'
#' To obtain an in memory data frame use an explict `collect()` or `st_as_sf()`.
#' A call to `collect()` is triggered by `st_as_sf()` and will add the sf class
#' to the output. A result may not contain a geometry column, and so cannot be
#' convert to an sf data frame. Using `collect()` on its own returns an
#' unclassed data.frame and may include a classed `sfc` geometry column.
#'
#' As well as `collect()` it's also possible to use `tibble::as_tibble()` or
#' `as.data.frame()` or `pull()` which all force computation and retrieve the
#' result.
#'
#' @inheritParams sf::read_sf
#' @param x the data source name (file path, url, or database connection string
#'   - analogous to [sf::read_sf()] 'dsn')
#' @param ... ignored
#' @param query SQL query to pass in directly
#' @return a 'tbl_SFSQLConnection', extending 'tbl_lazy' (something that works
#'   with dplyr verbs, and only shows a preview until you commit the result via
#'   [collect()]) see Details
#' @export
#'
#' @examples
#' # online sources can work
#' geojson <- file.path("https://raw.githubusercontent.com/SymbolixAU",
#'                      "geojsonsf/master/inst/examples/geo_melbourne.geojson")
#' \donttest{
#' lazysf(geojson)
#' }
#'
#' ## normal file stuff
#' ## (Geopackage is an actual database so with SELECT we must be explicit re geom-column)
#' f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' lazysf(f)
#' lazysf(f, query = "SELECT AREA, FIPS, geom FROM \"nc.gpkg\" WHERE AREA < 0.1")
#' lazysf(f, layer = "nc.gpkg") %>% dplyr::select(AREA, FIPS, geom) %>% dplyr::filter(AREA < 0.1)
#'
#' ## the famous ESRI Shapefile (not an actual database)
#' ## so if we SELECT we must be ex
#' shp <- lazysf(system.file("shape/nc.shp", package = "sf", mustWork = TRUE))
#' library(dplyr)
#' shp %>%
#'  filter(NAME %LIKE% 'A%') %>%
#'  mutate(abc = 1.3) %>%
#'  select(abc, NAME, `_ogr_geometry_`) %>%
#'  arrange(desc(NAME))  #%>% show_query()
#'
#'  ## a multi-layer file
#'  system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
lazysf <- function(x, layer, ...) {
  UseMethod("lazysf")
}
#' @name lazysf
#' @export
lazysf.character <- function(x, layer, ..., query = NA) {
  db <- dbConnect(SFSQL(), x)
  lazysf(db, layer, ..., query = query)
}
#' @name lazysf
#' @export
lazysf.SFSQLConnection <- function(x, layer, ..., query = NA) {
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



