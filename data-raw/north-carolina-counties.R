## copy the sf files for stable examples
ncfile <- system.file("shape/nc.shp", package = "sf", mustWork = TRUE)
gpkgfile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)

system(sprintf("ogr2ogr -nln north-carolina-counties %s %s nc.gpkg", file.path("inst/extdata", "north-carolina-counties.gpkg"), gpkgfile))

v <- new(gdalraster::GDALVector, ncfile)
f <- v$getFileList()
v$close()
file.copy(f,  file.path("inst/extdata", gsub("nc", "north-carolina-counties", basename(f))))
