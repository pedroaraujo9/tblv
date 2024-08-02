library(googledrive)
library(tidyverse)
library(yaml)

config = yaml.load_file("config.yaml")
drive_auth_configure(path = "credentials.json")

folder = "analysis/models"
gdrive_folder_id = config$gdrive_folder_id
models = drive_ls(path = as_id(gdrive_folder_id))
print(models)

for(i in 1:nrow(models)) {
  drive_download(as_id(models$id[i]),
                 path = file.path(folder, models$name[i]),
                 overwrite = TRUE)
}
