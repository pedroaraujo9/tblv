library(googledrive)
library(tidyverse)
library(yaml)
source("analysis/rscripts/utils.R")

config = yaml::yaml.load_file("config.yaml")

# download models 
download_models_gdrive(
  gdrive_auth_credentials = config$gdrive$auth_credentials, 
  gdrive_auth_email = config$gdrive$email, 
  gdrive_folder_id = config$gdrive$model_folder_id, 
  local_folder_path = "analysis/models"
)