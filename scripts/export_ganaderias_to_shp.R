
library(RSQLite)
library(tidyverse)
library(xlsx)
library(here)


con <- dbConnect(RSQLite::SQLite(), dbname = here::here("db/db_gps.db"))


# 
dbListTables(con)

# Datos de SN

sn <- dbGetQuery(con, 
                   "SELECT 
                   d.codigo_gps, d.lat, d.lng, d.month, 
                   g.id_ganadero, g.user_name, g.type,
                   DATETIME(d.time_stamp, 'unixepoch') as date
                   FROM 
                   datos_gps as d,
                   dicc_dispositivos as g
                   WHERE 
                   d.codigo_gps == g.codigo_gps")
# AND
#                    g.id_ganadero LIKE 'SNI%'")


library(sf)


for (i in unique(sn$id_ganadero)){ 
  aux <- sn %>% filter(id_ganadero == i) 
  
  aux_sp <- st_as_sf(aux, coords = c("lng","lat"), crs = 4326)
  
  name <- paste0("site_" , str_remove(i,pattern = "-"))
  st_write(aux_sp, paste0("/Users/ajpelu/Desktop/test_gps/",
                          name, ".shp"), 
           append = FALSE)
  assign(name, aux_sp)
}





