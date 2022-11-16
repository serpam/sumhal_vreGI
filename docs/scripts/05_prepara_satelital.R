#  ------------------------------------------------------------------------
# Title : Prepara datos gps satelital
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-11-01
#  ------------------------------------------------------------------------

library(here)
library(tidyverse)
library(fs)
library(glue)
library(gluedown)

# Set folder name (input data)
folder <- "descarga_aplicacion"

# Set date of download from application
fecha_descarga <- "2022_11_11"

# Set input folder
input_folder <- here::here(here(), paste0("rawdata/gps_sat/", folder))

# List all files with ..csv extension
file_names <- list.files(
  path = input_folder,
  full.names = TRUE, pattern = "*.txt"
)


prepareGPSsatelital <- function(x) {
  x |> 
    read_delim(delim = ";") |> 
    mutate(
      time_stamp =
        as.POSIXct(Fecha, format = "%d-%m-%Y%H:%M:%S"), 
      month = lubridate::month(time_stamp)) |> 
    dplyr::select(
      codigo_gps = Dispositivo, 
      lat = Latitud, 
      lng = Longitud,
      time_stamp, 
      month
    ) |> 
    unique()
}


g <- file_names %>%
  purrr::map(prepareGPSsatelital) %>%
  reduce(rbind)

# Summary 
st <- g |> group_by(codigo_gps) |> 
  summarise(
    nrecords = n(), 
    first_record = min(time_stamp),
    last_record = max(time_stamp)
  ) |> 
  separate(codigo_gps, into = c("cod", "gps"), remove = FALSE) |> 
  mutate(gps = as.numeric(gps)) |> 
  arrange(gps) |> 
  dplyr::select(-cod, -gps)

name_exported_file <- paste0("rawdata/gps_sat/", basename(input_folder), '_', fecha_descarga, '_', ".csv")

write_csv(g,
          file = 
            here::here(here(), name_exported_file))



# To create log file the first time
if (!(file.exists(here::here(here(), "rawdata/gps_sat/log_process_satelital.md")))) {
  file.create(here::here(here(), "rawdata/gps_sat/log_process_satelital.md"))
}

# For log
momentum <- Sys.time()
texto <- glue::glue(
  '# Log date {momentum}
  
  ## Files processed:
  - n = {length(file_names)}
  - **IDs** of the files processed: {glue::glue_collapse(basename(file_names), sep = ", ")}
  
  ## GPS devices processed: 
  - n = {length(unique(g$codigo_gps))}
  - {nrow(g)} records were processed
  - GPS devices: {glue::glue_collapse(unique(g$codigo_gps), sep = ", ")}
  
  ## Temporal coverage
  {md_table(st)}

  ### Output data: 
  - `{name_exported_file}`
  '
)


write(texto, here::here(here(), "rawdata/gps_sat/log_process_satelital.md"),
      append = TRUE
)








### Include historic data
# Set folder name (input data)
folder <- "historicos"

# Set date of download from application
fecha_descarga <- "historico"

# Set input folder
input_folder <- here::here(here(), paste0("rawdata/gps_sat/", folder))

# List all files with ..csv extension
file_names <- list.files(
  path = input_folder,
  full.names = TRUE, pattern = "*.csv"
)


prepareGPSsatelital_historico <- function(x) {
  x |> 
    read_delim(delim = ";") |> 
    mutate(
      time_stamp =
        as.POSIXct(track_time, format = "%Y-%m-%d%H:%M:%S"), 
      month = lubridate::month(time_stamp),
      codigo_gps = stringr::str_replace(messenger_name, "-", "_")) |> 
    dplyr::select(
      codigo_gps,
      lat = latitude, 
      lng = longitude,
      time_stamp, 
      month
    ) |> 
    unique()
}


h <- file_names |>
  purrr::map(prepareGPSsatelital_historico) |>
  reduce(rbind) |> 
  filter(codigo_gps != "GGPS_3")


# Summary 
sth <- h |> group_by(codigo_gps) |> 
  summarise(
    nrecords = n(), 
    first_record = min(time_stamp),
    last_record = max(time_stamp)
  ) |> 
  separate(codigo_gps, into = c("cod", "gps"), remove = FALSE) |> 
  mutate(gps = as.numeric(gps)) |> 
  arrange(gps) |> 
  dplyr::select(-cod, -gps)

name_exported_file <- paste0("rawdata/gps_sat/", basename(input_folder), '_', fecha_descarga, '_', ".csv")

write_csv(h,
          file = 
            here::here(here(), name_exported_file))



# To create log file the first time
if (!(file.exists(here::here(here(), "rawdata/gps_sat/log_process_satelital.md")))) {
  file.create(here::here(here(), "rawdata/gps_sat/log_process_satelital.md"))
}

# For log
momentum <- Sys.time()
texto <- glue::glue(
  '# Log date {momentum}
  
  ## Files processed:
  - n = {length(file_names)}
  - **IDs** of the files processed: {glue::glue_collapse(basename(file_names), sep = ", ")}
  
  ## GPS devices processed: 
  - n = {length(unique(h$codigo_gps))}
  - {nrow(h)} records were processed
  - GPS devices: {glue::glue_collapse(unique(h$codigo_gps), sep = ", ")}
  
  ## Temporal coverage
  {md_table(sth)}

  ### Output data: 
  - `{name_exported_file}`
  '
)


write(texto, here::here(here(), "rawdata/gps_sat/log_process_satelital.md"),
      append = TRUE
)



  





