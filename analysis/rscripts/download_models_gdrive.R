library(googledrive)
library(tidyverse)
library(yaml)

config = yaml::yaml.load_file("config.yaml")

googledrive::drive_deauth()
googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
googledrive::drive_auth(email = config$gdrive$email)

folder = "analysis/models"
model_folder_id = config$gdrive$model_folder_id
models = drive_ls(path = as_id(model_folder_id))
print(models)

for(i in 1:nrow(models)) {
  drive_download(as_id(models$id[i]),
                 path = file.path(folder, models$name[i]),
                 overwrite = TRUE)
}
