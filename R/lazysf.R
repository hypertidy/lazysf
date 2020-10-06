#' Delayed (lazy) read for simple features
#'
#' Really a lazy data frame for GDAL vector data sources. Currently we must use an
#' explict `st_as_sf()` on the `tbl_lazy` object. Using `collect()` returns an unclassed
#' data.frame possibly with a classed `sfc` geometry column.
#'
#' @inheritParams sf::read_sf
#' @param x the data source name (file path, url, or database connection string - analogous to [sf::read_sf()] 'dsn')
#' @param ... currently ignored
#' @return a 'tbl_SFSQLConnection', extending 'tbl_lazy' (something that works with dplyr verbs, and only shows a
#' preview until you commit the result via [dplyr::collect()])
#' @export
#'
#' @examples
#' # online sources can work
#' geojson <- file.path("https://raw.githubusercontent.com/SymbolixAU",
#'                      "geojsonsf/master/inst/examples/geo_melbourne.geojson")
#' lazysf(geojson)
#'
#' ## or normal stuff
#' f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' lazysf(f)
#' lazysf(f, query = "SELECT AREA, FIPS, geom FROM \"nc.gpkg\" WHERE AREA < 0.1")
#' lazysf(f, layer = "nc.gpkg") %>% dplyr::select(AREA, FIPS, geom) %>% dplyr::filter(AREA < 0.1)
#'
#' shp <- lazysf(system.file("shape/nc.shp", package = "sf", mustWork = TRUE))
#' library(dplyr)
#' shp %>%
#'  filter(NAME %LIKE% 'A%') %>%
#'  mutate(abc = 1.3) %>%
#'  select(abc, NAME, `_ogr_geometry_`) %>%
#'  arrange(desc(NAME))  #%>% show_query()
lazysf <- function(x, ...) {
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
#' Convert to in-memory sf
#'
#' Convert lazysf to an sf object
#'
#' WIP: do we need the st_as_sf.data.frame arguments?
#' @param x output of [lazysf()]
#' @param ... passed to [collect]
#' @name st_as_sf
#' @return a sf data frame (if possible, i.e. contains an `sfc` column on [collect()])
#' @export
#' @export st_as_sf
#' @importFrom dplyr collect
#' @importFrom sf st_as_sf
#' @examples
#' f <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)
#' lsf <- lazysf(f) %>% dplyr::select(AREA, FIPS, geom) %>% dplyr::filter(AREA < 0.1)
#' st_as_sf(lsf)
st_as_sf.tbl_SFSQLConnection <- function(x, ...) {
  sf::st_as_sf(dplyr::collect(x, ...))
}

