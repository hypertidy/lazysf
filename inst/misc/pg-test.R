library(lazysf)

con <- dbConnect(SFSQL_PG(),
  dbname = Sys.getenv("dbedu_dbname"),
  user = Sys.getenv("dbedu_usr"),
  password = Sys.getenv("dbedu_pwd"),
  host = Sys.getenv("dbedu_host")
)

library(dplyr)
mapa <- tbl(con, "mapa_comunas")

x <- mapa  %>% filter(codigo_provincia == "014") %>% select(codigo_comuna, geom) %>% st_as_sf()
