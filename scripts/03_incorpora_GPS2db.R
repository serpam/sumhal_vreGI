#  ------------------------------------------------------------------------
# Title : Incluir datos GPS en la base de datos   
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-07-05
#  ------------------------------------------------------------------------

# Load pkgs 
library(RSQLite)
library(tidyverse)
library(xlsx)
library(here)
library(glue)


# Set connection 
conn <- dbConnect(RSQLite::SQLite(), dbname = here::here("db/db_gps.db"))

# Custom function to append data into database
appendGPStoDB <- function(file, conn, table){ 
  
  gps_new <- read_csv(file) %>% 
    rename(codigo_gps = id_collar) %>% 
    as.data.frame()
  
  # Tenemos problemas con ceros en lat, long, y otras que presentan latitudes y 
  # longitudes algo extrañas. Solución --> filtrar por bbox (he creado un bbox de la P. Iberica)
  
  # Define the bounding box to filter out data 
  long_min = -9.49982
  long_max = 3.352826
  lat_min = 35.978678
  lat_max = 43.9933088
  
  g <- gps_new %>% 
    filter(between(lng, long_min, long_max)) %>% 
    filter(between(lat, lat_min, lat_max))
  
  RSQLite::dbWriteTable(conn, table, g, append = TRUE, row.names = FALSE)
  
  
  # For log
  # To create log file the first time 
  if(!(file.exists(here::here(here(), "db/log_incorporate.txt")))){
    file.create(here::here(here(), "db/log_incorporate.txt"))
  }
  
  momentum <- Sys.time()
  texto <- glue::glue('
  ## Log date {momentum}
  ## File processed: {basename(file)}
  ## Date period: 
  ### First record date: {min(g$time_stamp)} 
  ### Last record date: {max(g$time_stamp)}
  ## Data incorporated: 
  ### GPS devices: {length(unique(g$codigo_gps))} 
  ### Users devices: {length(unique(g$id_user))} users
  ## {nrow(g)} records were incorporated into the database. {nrow(gps_new) - nrow(g)} records were filtered out due to spatial errors
  ### GPS devices:
  # {glue::glue_collapse(unique(g$codigo_gps), sep = ", ")}
  =============================================================')
  
  write(texto, here::here(here(), "db/log_incorporate.txt"), append = TRUE)
  
}






myfile <- here::here("rawdata/gps_mail/all_gps_2022_05.csv")

appendGPStoDB(file = myfile, conn = conn, table = "datos_gps") 

RSQLite::dbDisconnect(conn)


















gps_new <- read_csv(myfile) %>%
  rename(codigo_gps = id_collar) %>% 
  as.data.frame()

# Tenemos problemas con ceros en lat, long, y otras que presentan latitudes y 
# longitudes algo extrañas. Solución --> filtrar por bbox (he creado un bbox de la P. Iberica)

# Define the bounding box to filter out data 
long_min = -9.49982
long_max = 3.352826
lat_min = 35.978678
lat_max = 43.9933088

g <- gps_new %>% 
  filter(between(lng, long_min, long_max)) %>% 
  filter(between(lat, lat_min, lat_max))

gps_new <- read_csv(here::here("rawdata/gps_mail/all_gps_2022_04.csv")) %>% 
  rename(codigo_gps = id_collar)

dbWriteTable(con, "datos_gps", gps_new, append = TRUE, row.names = FALSE)


