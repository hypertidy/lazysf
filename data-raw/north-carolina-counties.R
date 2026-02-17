## copy the sf files for stable examples
ncfile <- system.file("shape/nc.shp", package = "sf", mustWork = TRUE)
gpkgfile <- system.file("gpkg/nc.gpkg", package = "sf", mustWork = TRUE)


file.copy(gpkgfile, file.path("inst/extdata", "north-carolina-counties.gpkg"))

## nope and nope
#file.copy(ncfile, file.path("inst/extdata", "north-carolina-counties.shp"))
#system(sprintf("gdal vsi copy %s %s", ncfile, file.path("inst/extdata", "north-carolina-counties.shp")))
## yes
v <- new(gdalraster::GDALVector, ncfile)
f <- v$getFileList()
v$close()
file.copy(f,  file.path("inst/extdata", gsub("nc", "north-carolina-counties", basename(f))))
