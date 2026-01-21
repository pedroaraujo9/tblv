library(googledrive)
library(tidyverse)
library(yaml)
source("analysis/rscripts/utils.R")

config = yaml::yaml.load_file("config.yaml")

gdrive_auth_credentials = config$gdrive$auth_credentials
gdrive_auth_email = config$gdrive$email
gdrive_folder_id = config$gdrive$model_folder_id
local_folder_path = "analysis/models"

googledrive::drive_deauth()
googledrive::drive_auth_configure(path = gdrive_auth_credentials)
googledrive::drive_auth(email = gdrive_auth_email)

models = googledrive::drive_ls(path = as_id(gdrive_folder_id)) %>%
  dplyr::arrange(name)

models = models %>%
  filter(str_detect(name, "btblv-qx-complete"))

models

for(i in 1:nrow(models)) {
  googledrive::drive_download(googledrive::as_id(models$id[i]),
                              path = file.path(local_folder_path, models$name[i]),
                              overwrite = TRUE)
}

