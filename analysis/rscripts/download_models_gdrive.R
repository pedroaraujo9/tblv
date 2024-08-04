library(googledrive)
library(tidyverse)
library(yaml)

config = yaml::yaml.load_file("config.yaml")
googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)

folder = "analysis/models"
model_folder_id = config$gdrive$model_folder_id
models = drive_ls(path = as_id(model_folder_id))
print(models)

for(i in 1:nrow(models)) {
  drive_download(as_id(models$id[i]),
                 path = file.path(folder, models$name[i]),
                 overwrite = TRUE)
}
