#' Fit and Save a BTBLV Model
#'
#' This function fits a Bayesian Tree-Based Latent Variable (BTBLV) model, calculates metrics,
#' and saves the results either locally or to Google Drive. It handles authentication and ensures
#' the model is not re-fitted if already saved.
#'
#' @param btblv_data_path Character. Path to the RDS file containing the BTBLV data.
#' @param K Integer. Number of clusters or latent variables to fit in the model.
#' @param iter Integer. Number of iterations for the sampler.
#' @param warmup Integer. Number of warmup iterations for the sampler.
#' @param thin Integer. Thinning interval for the sampler.
#' @param chains Integer. Number of chains for the sampler.
#' @param precision Character. Precision parameter for the model.
#' @param seed Integer. Seed for random number generation.
#' @param mc_samples Integer. Number of Monte Carlo samples for posterior predictive metrics.
#' @param config_path Character. Path to the configuration file (YAML format) for authentication and other settings.
#' @param model_name_pattern Character. Pattern for generating the model name.
#' @param save_gdrive Logical. If `TRUE`, saves the model to Google Drive.
#' @param gdrive_folder_id Character. Google Drive folder ID to save the model. Required if `save_gdrive` is `TRUE`.
#' @param local_path Character. Local directory path to save models and temporary files.
#' @param max_treedepth Integer. Maximum tree depth for Stan's Hamiltonian Monte Carlo sampler. Default is 10.
#' @param ... Additional arguments passed to the `btblv::fit_btblv` function.
#'
#' @return Logical. `TRUE` if the model was fitted and saved successfully, `FALSE` if the model was already saved.
#'
#' @details
#' - Before fitting, the function checks if a model with the same name already exists locally or on Google Drive.
#' - If `save_gdrive` is `TRUE`, Google Drive authentication is performed using credentials specified in the YAML configuration file.
#' - The model fitting process includes posterior predictive metric calculations using the specified number of Monte Carlo samples.
#'
#' @examples
#' \dontrun{
#'   fit_save_btblv(
#'     btblv_data_path = "data/btblv_data.rds",
#'     K = 3,
#'     iter = 2000,
#'     warmup = 1000,
#'     thin = 1,
#'     chains = 4,
#'     precision = 0.01,
#'     seed = 42,
#'     mc_samples = 100,
#'     config_path = "config.yaml",
#'     model_name_pattern = "model_K",
#'     save_gdrive = TRUE,
#'     gdrive_folder_id = "1A2B3C4D5E6F7G8H9I",
#'     local_path = "output",
#'     max_treedepth = 12
#'   )
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom yaml yaml.load_file
#' @importFrom googledrive drive_deauth drive_auth_configure drive_auth drive_upload as_id
#' @importFrom stringr str_flatten
#' @export
fit_save_btblv = function(btblv_data_path,
                          K,
                          iter,
                          warmup,
                          thin,
                          chains,
                          precision,
                          seed,
                          mc_samples,
                          config_path,
                          model_name_pattern,
                          save_gdrive,
                          gdrive_folder_id,
                          local_path,
                          max_treedepth = 10,
                          ...) {

  #### Google drive authentication ####
  if(save_gdrive == TRUE) {

    config = yaml::yaml.load_file(config_path)

    googledrive::drive_deauth()
    
    if(!is.null(config$gdrive$auth_credentials)) {
      googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
    }
    
    googledrive::drive_auth(email = config$gdrive$email)

  }

  #### read data ####
  btblv_data = readRDS(btblv_data_path)

  #### create model name ####
  model_name = .get_model_name(model_name_pattern, K, precision)

  cat(paste0("\n\n----- MODEL NAME -----\n\n", model_name, "\n"))

  #### check if the model was fitted before ####
  models_saved = .get_models_saved(save_gdrive, local_path, gdrive_folder_id)

  cat(paste0("\n\n----- MODELS SAVED -----\n\n"))
  models_saved |>
    stringr::str_flatten("\n") |>
    cat()

  #### model fit ####
  if(!(model_name %in% models_saved)) {

    btblv_fit = btblv::fit_btblv(
      btblv_data = btblv_data,
      precision = precision,
      K = K,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      cores = chains,
      seed = seed,
      open_progress = FALSE,
      control = list(max_treedepth = max_treedepth),
      ...
    )

    metrics = .compute_model_metrics(
      btblv_fit, mc_samples = mc_samples, cores = chains, seed = seed
    )

    fit = list(
      btblv_fit = btblv_fit,
      metrics = metrics
    )

    if(save_gdrive == TRUE) {

      saveRDS(fit, paste0(local_path, "/temp-", model_name))

      googledrive::drive_upload(
        media = paste0(local_path, "/temp-", model_name),
        name = model_name,
        path = googledrive::as_id(gdrive_folder_id),
        overwrite = TRUE
      )

      file.remove(paste0(local_path, "/temp-", model_name))

    }else{

      # updating list with models saved
      saveRDS(fit, paste0(local_path, "/", model_name))

      model_name |>
        c(models_saved) |>
        writeLines(paste0(local_path, "/models-saved-list.txt"))

    }

    return(TRUE)

  }else{
    cat("\n\n----- FIT ALREADY EXISTS. FIT NOT DONE! -----\n\n")
    return(FALSE)
  }
}
