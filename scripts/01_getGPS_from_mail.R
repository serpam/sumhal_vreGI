#  ------------------------------------------------------------------------
# Title : Get GPS Data from mail  
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-03-18
#  ------------------------------------------------------------------------

# Get attachment from MAIL 
library(mRpostman) # An IMAP Client for R, CRAN v1.0.0
library(here) 
library(tidyverse)

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
f <- "29-Apr-2022"

# Get attachment
con$search(request = AND(string(expr = "support@digitanimal.com", 
                                where = "FROM"),
                         since(date_char = f))) %>% 
  con$fetch_body(write_to_disk = TRUE) %>% 
  con$get_attachments(override = TRUE)







