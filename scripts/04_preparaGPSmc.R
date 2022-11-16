#  ------------------------------------------------------------------------
# Title : Prepara datos gps MC
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-03-18
#  ------------------------------------------------------------------------

library(here)
library(tidyverse)
library(fs)
library(glue)

# Set folder name (input data)
folder <- "220601_Datos"

# Set input folder
input_folder <- here::here(here(), paste0("rawdata/gps_mc/", folder))

# List all files with ..csv extension
file_names <- list.files(
  path = input_folder,
  full.names = TRUE, pattern = "*.csv"
)

prepareGPSmc <- function(x) {
  x %>%
    read_csv() %>%
    mutate(
      time_stamp =
        lubridate::make_datetime(as.numeric(paste0("20", year)),
          month, day, hour,
          min = minute, sec = second
        ),
      id_itm = NA, id_user = NA
    ) %>%
    dplyr::select(
      codigo_gps = id_gps,
      id_itm, id_user, lat, lng = long, time_stamp, month
    )
}

g <- file_names %>%
  purrr::map(prepareGPSmc) %>%
  reduce(bind)

write_csv(g,
  file =
    here::here(here(), paste0(
      "rawdata/gps_mc/",
      basename(input_folder), ".csv"
    ))
)

# For log
# To create log file the first time
if (!(file.exists(here::here(here(), "rawdata/gps_mc/log_preaparaGPSmc.txt")))) {
  file.create(here::here(here(), "rawdata/gps_mc/log_preaparaGPSmc.txt"))
}

momentum <- Sys.time()
texto <- glue::glue('
  ## Log date {momentum}
  ## N Files processed: {length(file_names)}
  ## Date period:
  ### First record date: {min(g$time_stamp)}
  ### Last record date: {max(g$time_stamp)}
  ## Data incorporated:
  ### GPS devices: {length(unique(g$codigo_gps))}
  # {glue::glue_collapse(unique(g$codigo_gps), sep = ", ")}
  ### File exported as {paste0("rawdata/gps_mc/", basename(input_folder), ".csv")}
  =============================================================')

write(texto, here::here(here(), "rawdata/gps_mc/log_preaparaGPSmc.txt"), append = TRUE)
