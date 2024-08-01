library(googledrive)
library(tidyverse)

drive_auth_configure(path = "credentials.json")

folder = "analysis/models"
gdrive_folder_id = "1qlT8pJsHhzQF8-XCa7bS-f5fmEwcEIcB"
models = drive_ls(path = as_id(gdrive_folder_id))
print(models)

for(i in 1:nrow(models)) {
  drive_download(as_id(models$id[i]),
                 path = file.path(folder, models$name[i]),
                 overwrite = TRUE)
}
