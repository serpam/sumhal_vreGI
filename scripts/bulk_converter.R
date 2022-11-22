library(tidyverse)
library(purrr)
library(lubridate)


# Leer todas los archivos csv 
input_folder <- "/Users/ajpelu/Desktop/Serrato"


# Genera un objeto con los nombres (ruta) de los archivos txt
a <- list.files(path = input_folder, full.names = TRUE, pattern = ".txt")

# Funcion para convertir 
gps_converter <- function(x){
  x %>% 
    read.csv(header=FALSE, sep = ",", skip = 1) %>% 
    mutate(id_gps = str_remove(basename(x), ".txt")) %>% 
    dplyr::select_if(~ !all(is.na(.))) %>% 
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
    mutate(date = lubridate::make_date(as.numeric(paste0("20", year)), month, day),
           date_time = lubridate::make_datetime(as.numeric(paste0("20", year)), month, day, 
                                                hour, min = minute, sec=second))
}


# Generar un dataframe con todos los datos 
data <- a %>%
  map(gps_converter) %>% 
  reduce(rbind)

write_csv(data, file = paste0(input_folder,"/todos.csv"))











