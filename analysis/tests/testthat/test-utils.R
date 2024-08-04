# create directory for tests
temp_test_dir = "save_test"
unlink(temp_test_dir, recursive = TRUE)
dir.create(temp_test_dir)

# clean gdrive folder
config_path = "../../../config.yaml"
config = yaml::yaml.load_file(config_path)

googledrive::drive_deauth()
googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
googledrive::drive_auth(email = config$gdrive$email)

gdrive_folder_id = config$gdrive$dev_model_folder_id
gdrive_files = googledrive::drive_ls(googledrive::as_id(gdrive_folder_id))
googledrive::drive_rm( googledrive::as_id(gdrive_files$id))

# fixed parameters
K = 2
iter = 10 
warmup = 5
thin = 1 
chains = 2
data_path = "../../data/data_model.rds"
local_save_path = temp_test_dir

# tests
testthat::test_that("inputs", {
  
  for(K in c(1:2)) {
    
    model_name_pattern = NULL
    precision = "single"
    save_gdrive = FALSE
    
    #### local ####
    expect_no_error(
      
      save_fit_btblv(
        K = K, 
        iter = iter, 
        warmup = warmup, 
        thin = thin, 
        chains = chains,
        seed = 1, 
        precision = precision, 
        config_path = config_path, 
        model_name_pattern = model_name_pattern,
        save_gdrive = save_gdrive,
        data_path = data_path,
        local_save_path = local_save_path, 
        gdrive_folder_id = gdrive_folder_id,
        refresh = 0,
        show_messages = FALSE,
        verbose = FALSE,
        open_progress = FALSE
      ) %>%
        suppressWarnings()
      
    )
    
    #### should not refit the models that already exists ####
    expect_false(
      
      save_fit_btblv(
        K = K, 
        iter = iter, 
        warmup = warmup, 
        thin = thin, 
        chains = chains,
        seed = 1, 
        precision = precision, 
        config_path = config_path, 
        model_name_pattern = model_name_pattern,
        save_gdrive = save_gdrive,
        data_path = data_path,
        local_save_path = local_save_path, 
        gdrive_folder_id = gdrive_folder_id,
        refresh = 0,
        show_messages = FALSE,
        verbose = FALSE,
        open_progress = FALSE
      ) %>%
        suppressWarnings()
    )
    
    #### local different name ####
    model_name_pattern = "trueK=3-repl=1"
    
    expect_no_error(
      
      save_fit_btblv(
        K = K, 
        iter = iter, 
        warmup = warmup, 
        thin = thin, 
        chains = chains,
        seed = 1, 
        precision = precision, 
        config_path = config_path, 
        model_name_pattern = model_name_pattern,
        save_gdrive = save_gdrive,
        data_path = data_path,
        local_save_path = local_save_path, 
        gdrive_folder_id = gdrive_folder_id,
        refresh = 0,
        show_messages = FALSE,
        verbose = FALSE,
        open_progress = FALSE
      ) %>%
        suppressWarnings()
    )
    
    #### Google drive ####
    save_gdrive = TRUE
    model_name_pattern = NULL
    
    expect_no_error(
      
      save_fit_btblv(
        K = K, 
        iter = iter, 
        warmup = warmup, 
        thin = thin, 
        chains = chains,
        seed = 1, 
        precision = precision, 
        config_path = config_path, 
        model_name_pattern = model_name_pattern,
        save_gdrive = save_gdrive,
        data_path = data_path,
        local_save_path = local_save_path, 
        gdrive_folder_id = gdrive_folder_id,
        refresh = 0,
        show_messages = FALSE,
        verbose = FALSE,
        open_progress = FALSE
      ) %>%
        suppressWarnings()

    )
    
  }
})

test_that("outputs", {
  
  files = list.files(temp_test_dir)
  files_txt = readLines(paste0(temp_test_dir, "/models-saved-list.txt"))
  
  saved = c(
    paste0("btblv-precision=single-K=", 1:2, ".rds"),
    paste0("btblv-trueK=3-repl=1-precision=single-K=", 1:2, ".rds")
  )
  
  files %>%
    expect_contains(saved)
  
  files %>%
    expect_contains(files_txt)
  
  files_gdrive = googledrive::drive_ls(googledrive::as_id(gdrive_folder_id))$name
  
  files_gdrive %>%
    expect_contains(paste0("btblv-precision=single-K=", 1:2, ".rds"))
  
})



