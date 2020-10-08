library(lazysf)

con <- dbConnect(SFSQL_PG(),
  dbname =
  user =
  password =
  host =
)

library(dplyr)
mapa <- tbl(con, "mapa_comunas")

x <- mapa  %>% filter(codigo_provincia == "014") %>% select(codigo_comuna, geom) %>% st_as_sf()




```{r postgres}

(sfx <- lazysf(con, query = "SELECT st_area(st_transform(geom, '+proj=laea +lon_0=-68 +lat_0=-25')) / 1e6 AS sq_km,
                                        codigo_comuna, geom FROM mapa_comunas WHERE codigo_region = '03'"))
```

You can construct the database connection string yourself and provide it
directly to lazysf() if you want. See [GDAL Postgres](https://gdal.org/drivers/vector/pg.html)
for details on this format, and this capability is available for any of the GDAL formats (see limitations above).
