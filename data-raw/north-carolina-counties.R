## copy the sf files for stable examples
ncfile <- system.file("shape/nc.shp", package = "sf", mustWork = TRUE)
gpkgfile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)

system(sprintf("ogr2ogr -nln nc %s %s nc.gpkg", file.path("inst/extdata", "nc.gpkg"), gpkgfile))
system(sprintf("ogr2ogr -nln nc %s %s nc", file.path("inst/extdata", "nc.shp"), ncfile))

