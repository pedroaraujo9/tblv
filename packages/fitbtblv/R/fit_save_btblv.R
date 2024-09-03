#' Fit btblv model and save it locally or in a Google drive folder
#'
#' @param btblv_data_path string with path to the `btblv::btblv_data` object.
#' @param K integer with the latent dimension size.
#' @param iter integer with the number of iterations
#' @param warmup integer with the warm-up size. See `rstan::sampling`.
#' @param thin integer with the thinning size. See `rstan::sampling`.
#' @param chains integer with the number of chains. See `rstan::sampling`.
#' @param seed integer with the random seed.
#' @param mc_samples integer with the Monte Carlo samples for the marginal likelihood approximation.
#' @param precision string with the type of precision. It is "single" if all
#' items have the same precision or "specific" if each item has its own precision
#' parameter. Default is "single".
#' @param config_path string with the path to the config.yaml with credentials for google drive.
#' Default is `NULL`.
#' @param model_name_pattern string value with characters to be added to the model name.
#' It will be used as the name of the final file to be saved.
#' @param save_gdrive logical value. If `TRUE` saves the model on Google drive.
#' If `FALSE` saves the model locally.
#' @param gdrive_folder_id string with the Google drive folder id.
#' @param local_path string with the path of the folder used to store local data.
#' It also needed for the Google drive saving.
#' @param ... additional `rstan::sampling` parameters.
#'
#' @return `TRUE` if the model was saved, `FALSE` otherwise.
#'
#' @export
#'
#' @import googledrive
#' @import btblv
#' @importFrom stringr str_flatten
#'
#' @examples
#' ##
#'
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
                          ...) {

  #### Google drive authentication ####
  if(save_gdrive == TRUE) {

    config = yaml::yaml.load_file(config_path)

    googledrive::drive_deauth()
    googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
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
  models_saved %>%
    stringr::str_flatten("\n") %>%
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

      model_name %>%
        c(models_saved) %>%
        writeLines(paste0(local_path, "/models-saved-list.txt"))

    }

    return(TRUE)

  }else{
    cat("\n\n----- FIT ALREADY EXISTS. FIT NOT DONE! -----\n\n")
    return(FALSE)
  }
}
