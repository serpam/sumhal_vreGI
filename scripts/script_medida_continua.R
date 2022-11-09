library(tidyverse)
library(hms)

path <- "/Users/ajpelu/Nextcloud/sumhal/sumhal_vreGI/rawdata/GPS_MC"

# Leer todos los archivos 
f <- list.files(path = path, pattern = "*.txt", 
                full.names = TRUE, recursive = TRUE, include.dirs = TRUE)



formateaGPSdata <- function(x) {
  
  require(tidyverse)
  require(hms)

  out <- read.delim(file = x, sep = ",", 
                    header = FALSE, skip = 1) %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    mutate(id_gps = gsub(".txt", "", basename(x))) %>%
    rename(
      id = V1,
      year = V11,
      month = V12,
      day = V13,
      hour = V14,
      minute = V15,
      second = V16,
      lat = V17,
      long = V18
    ) %>%
    filter(lat != -1 | long != -1) %>%
    mutate(
      date = as.POSIXct(paste(year, month, day, sep = "-"), format = "%y-%m-%d"),
      time = hms::as_hms(paste(hour, minute, second, sep = ":"))
    ) %>%
    mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(id = as.integer(id)) %>% 
    dplyr::select(id, id_gps, lat, long, datetime, date, time)
  
  return(out)
}



dddd <- purrr::map_dfr(.x = f, 
                .f = ~formateaGPSdata(x = .x))



write_csv(dddd, file="/Users/ajpelu/Nextcloud/sumhal/sumhal_vreGI/rawdata/GPS_MC/all.csv")

# Leer el archivo,
# Saltar la primera línea
# Eliminar las columnas vacías
# Añadir nombre archivo



purrr::map_dfr(f, formateaGPSdata)



g <- formateaGPSdata(f[1], path=path)

         

library(sf)
s <- st_as_sf(dddd, coords = c("long", "lat"), crs = 4326)
st_write(s, dsn=paste0(path, "prueba.shp"))
  



"Lanjaron/AV601/AV601.txt"


mutate(id_gps = gsub(".txt", "", x)) %>%






