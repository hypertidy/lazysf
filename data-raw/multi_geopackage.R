library(sf)
d <- ozmaps::ozmap_states
d2 <- sf::st_cast(d, "POLYGON") %>% st_centroid() %>% dplyr::rename(state = NAME)
st_write(d, driver = "GPKG", dsn = "inst/extdata/multi.gpkg", layer = "state")
st_write(d2, driver = "GPKG", dsn = "inst/extdata/multi.gpkg", layer = "centre")

#system.file("extdata/multi.gpkg", package = "lazysf", mustWork = TRUE)
