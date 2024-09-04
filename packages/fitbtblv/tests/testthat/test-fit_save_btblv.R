test_that("inputs", {

  dir.create("models_test")

  iter = 30
  warmup = 15
  thin = 1
  chains = 2
  precision = "single"
  config_path = "config.yaml"
  gdrive_folder_id = "1LvmQrUG3P424ZsEVi_LuWfgUHoLmf-LH"
  mc_samples = 100
  local_path = "models_test"

  data("qx_btblv_data")
  data("mx_btblv_data")

  saveRDS(qx_btblv_data, "models_test/qx_btblv_data.rds")
  saveRDS(mx_btblv_data, "models_test/mx_btblv_data.rds")

  if(file.exists("config.yaml")) {



    fit_save_btblv(
      btblv_data_path = "models_test/qx_btblv_data.rds",
      K = 2,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      precision = precision,
      seed = 1,
      mc_samples = mc_samples,
      config_path = "config.yaml",
      model_name_pattern = "qx",
      save_gdrive = T,
      gdrive_folder_id = gdrive_folder_id,
      local_path = local_path,
      refresh = 0,
      show_messages = FALSE,
      verbose = FALSE
    ) %>%
      suppressWarnings() %>%
      expect_no_error()

    fit_save_btblv(
      btblv_data_path = "models_test/qx_btblv_data.rds",
      K = 2,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      precision = precision,
      seed = 1,
      mc_samples = mc_samples,
      config_path = "config.yaml",
      model_name_pattern = "qx",
      save_gdrive = T,
      gdrive_folder_id = gdrive_folder_id,
      local_path = local_path,
      refresh = 0,
      show_messages = FALSE,
      verbose = FALSE
    ) %>%
      suppressWarnings() %>%
      expect_false()

    fit_save_btblv(
      btblv_data_path = "models_test/mx_btblv_data.rds",
      K = 2,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      precision = precision,
      seed = 1,
      mc_samples = mc_samples,
      config_path = "config.yaml",
      model_name_pattern = "mx",
      save_gdrive = T,
      gdrive_folder_id = gdrive_folder_id,
      local_path = local_path,
      refresh = 0,
      show_messages = FALSE,
      verbose = FALSE
    ) %>%
      suppressWarnings() %>%
      expect_no_error()

    config = yaml::yaml.load_file(config_path)

    googledrive::drive_deauth()
    googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
    googledrive::drive_auth(email = config$gdrive$email)

    saved_ids = googledrive::drive_ls(googledrive::as_id(gdrive_folder_id))
    googledrive::drive_rm(googledrive::as_id(saved_ids$id))

  }else{
    cat("\n")
    cat("config.yaml file not available, skiping GDrive tests.")
  }

  fit_save_btblv(
    btblv_data_path = "models_test/mx_btblv_data.rds",
    K = 2,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = precision,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "",
    model_name_pattern = "mx",
    save_gdrive = FALSE,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path,
    refresh = 0,
    show_messages = FALSE,
    verbose = FALSE
  ) %>%
    suppressWarnings() %>%
    expect_no_error()

  # should not re-run
  fit_save_btblv(
    btblv_data_path = "models_test/mx_btblv_data.rds",
    K = 2,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = precision,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "",
    model_name_pattern = "mx",
    save_gdrive = FALSE,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path,
    refresh = 0,
    show_messages = FALSE,
    verbose = FALSE
  ) %>%
    suppressWarnings() %>%
    expect_false()


  unlink("models_test", recursive = TRUE)

})
