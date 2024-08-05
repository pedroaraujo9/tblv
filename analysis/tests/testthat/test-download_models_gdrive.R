config_path = "../../../config.yaml"
config = yaml::yaml.load_file(config_path)

googledrive::drive_deauth()
googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
googledrive::drive_auth(email = config$gdrive$email)

# create dir for test
temp_test_dir = "save_test"
unlink(temp_test_dir, recursive = TRUE)
dir.create(temp_test_dir)

# save file 
data.frame(
  x = rnorm(1000)
) %>% 
  saveRDS("save_test/test_file.rds")

# upload file 
googledrive::drive_upload(
  "save_test/test_file.rds", 
  path = googledrive::as_id(config$gdrive$dev_model_folder_id)
)

# delete file 
file.remove("save_test/test_file.rds")

# test
test_that("inputs", {
  
  expect_no_error(
    
    download_models_gdrive(
      gdrive_auth_credentials = config$gdrive$auth_credentials, 
      gdrive_auth_email = config$gdrive$email, 
      gdrive_folder_id = config$gdrive$dev_model_folder_id, 
      local_folder_path = "save_test"
    ) 

  )

})

test_that("outputs", {
  list.files("save_test") %>%
    expect_contains("test_file.rds")
})
