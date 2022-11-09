#  ------------------------------------------------------------------------
# Title : Get GPS Data from mail  
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-03-18
#  ------------------------------------------------------------------------

# Get attachment from MAIL 
library(mRpostman) # An IMAP Client for R, CRAN v1.0.0
library(here) 
library(tidyverse)
library(fs)
library(logr)
library(glue)


# Set dir 
setwd(here::here("rawdata/gps_mail")) 

# Configure conection 
con <- configure_imap(
  url = "imaps://correo.csic.es",
  username = rstudioapi::askForSecret(name= "User", message = "provide your mail user"),
  password=rstudioapi::askForPassword()
)

# Select INBOX 
con$select_folder(name = "INBOX") 

# Set the filter for date 
# emails are received on the first month's day, so we search after the lasts 
# days of the previous month
start <- "27-May-2022"
end <- "29-Jun-2022"
name_folder <- "2022_05"

# Get attachment
con$search(request = AND(string(expr = "support@digitanimal.com", 
                                where = "FROM"),
                         since(date_char = start), 
                         before(date_char = end))) %>% 
  con$fetch_body(write_to_disk = TRUE) %>% 
  con$get_attachments(override = TRUE)

# How many mails were read and extracted 
file_names <- list.files(path = here::here(here(), "rawdata/gps_mail/eez_serpam/INBOX"), 
                         pattern = "*.txt")

# Move all data to a specific folder 
fs::dir_copy(here::here(here(), "rawdata/gps_mail/eez_serpam/INBOX"),
             here::here(here(), paste0("rawdata/gps_mail/eez_serpam/",name_folder)))

# Remove all data 
fs::dir_delete(here::here(here(), "rawdata/gps_mail/eez_serpam/INBOX"))


# To create log file the first time 
if(!(file.exists(here::here(here(), "rawdata/gps_mail/eez_serpam/log_getfrommail.txt")))){
  file.create(here::here(here(), "rawdata/gps_mail/eez_serpam/log_getfrommail.txt"))
}

# For log
momentum <- Sys.time()
texto <- glue::glue(
  '
## Log date {momentum}
## Period: the gps data corresponds to {name_folder}
## Files extracted from mail account: {length(file_names)} 
### IDs of mail extracted: 
{glue::glue_collapse(str_remove(str_remove(file_names,"body"), ".txt"),sep = ", ")}.
=============================================================')

write(texto, here::here(here(), "rawdata/gps_mail/eez_serpam/log_getfrommail.txt"), 
      append = TRUE)
