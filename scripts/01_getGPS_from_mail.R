#  ------------------------------------------------------------------------
# Title : Get GPS Data from mail  
#    By : AJ Pérez-Luque @ajpelu
#  Date : 2022-03-18
#  ------------------------------------------------------------------------

# Get attachment from MAIL 
library(mRpostman) # An IMAP Client for R, CRAN v1.0.0
library(here) 

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

# Get attachment
con$search_string(expr = "support@digitanimal.com", where = "FROM") %>% 
  con$fetch_body(write_to_disk = TRUE) %>% 
  con$get_attachments(override = TRUE)





