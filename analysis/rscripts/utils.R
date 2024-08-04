library(tidyverse)
library(btblv)

#' Fit btblv model and save it locally or in a google drive folder
#'
#' @param K integer with the latent dimension size.
#' @param iter integer with the number of iterations
#' @param warmup integer with the warm-up size. See `rstan::sampling`.
#' @param thin integer with the thinning size. See `rstan::sampling`.
#' @param chains integer with the number of chains. See `rstan::sampling`.
#' @param seed integer with the random seed.
#' @param precision string with the type of precision. It is "single" if all
#' items have the same precision or "specific" if each item has its own precision
#' parameter. Default is "single".
#' @param config_path string with the path to the config.yaml with credentials for google drive. 
#' Default is `NULL`.
#' @param model_name_pattern string value with characters to be added to the model name.
#' It will be used as the name of the final file to be saved.
#' @param save_gdrive logical value. If `TRUE` saves the model on google drive. 
#' If `FALSE` saves the model locally
#' @param data_path string for the path where the `.rds` data is locallated.
#' @param local_save_path string with the path of the folder used to store local data.
#' @param gdrive_folder_id string with the google drive folder id.
#'
#' @return `NULL`
#'
#' @examples
#' 
save_fit_btblv = function(K, 
                          iter, 
                          warmup, 
                          thin, 
                          chains,
                          seed, 
                          precision, 
                          config_path = NULL, 
                          model_name_pattern,
                          save_gdrive,
                          data_path,
                          local_save_path, 
                          gdrive_folder_id,
                          ...) {
  
  #### config ####
  if(!is.null(config_path)) {
    config = yaml::yaml.load_file(config_path)
  }
  
  #### read data ####
  df = readRDS(data_path)
  
  #### Google drive authentication #### 
  if(save_gdrive == TRUE) {
    
    googledrive::drive_deauth()
    googledrive::drive_auth_configure(path = config$gdrive$auth_credentials)
    googledrive::drive_auth(email = config$gdrive$email)
    
  }
  
  #### create model name ####
  if(is.null(model_name_pattern)){
    
    model_name = paste0(
      "btblv-precision=", precision, "-",
      "K=", K, ".rds"
    )
    
  }else{
    
    if(model_name_pattern == "") {
      model_name = paste0(
        "btblv-precision=", precision, "-",
        "K=", K, ".rds"
      )
    }
    
    model_name = paste0(
      "btblv-", model_name_pattern, "-precision=", precision, "-",
      "K=", K, ".rds"
    )
  }
  
  cat(paste0("\nMODEL NAME: ", model_name, "\n"))
  
  #### check if the model was fitted before ####
  
  ##### local #####
  if(save_gdrive == FALSE) {
    models_saved_path = paste0(local_save_path, "/models-saved-list.txt") 
    
    if(!file.exists(models_saved_path)) {
      file.create(models_saved_path)
    }
    
    models_saved = readLines(models_saved_path)
    print(models_saved)
    
    ##### Google drive #####
  }else{
    models_saved = googledrive::drive_ls(path = googledrive::as_id(gdrive_folder_id))
    models_saved = models_saved$name
    print(models_saved)
  }
  
  #### model fit ####
  if(!(model_name %in% models_saved)) {
    
    data = btblv::create_btblv_data(
      df = df,
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    )
    
    fit = btblv::fit_btblv(
      btblv_data = data,
      precision = precision,
      K = K,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      cores = chains,
      seed = seed,
      ...
    )
    
    if(save_gdrive == TRUE) {
      saveRDS(fit, paste0(local_save_path, "/temp-", model_name))
      
      googledrive::drive_upload(media = paste0(local_save_path, "/temp-", model_name), 
                                name = model_name,
                                path = googledrive::as_id(gdrive_folder_id),
                                overwrite = TRUE)
      
      file.remove(paste0(local_save_path, "/temp-", model_name))
      
    }else{
      # updating list with models saved
      saveRDS(fit, paste0(local_save_path, "/", model_name))
      
      c(model_name, models_saved) %>%
        writeLines(paste0(local_save_path, "/models-saved-list.txt"))
    }
    return(TRUE)
  }else{
    print("FIT ALREADY EXISTS. FIT NOT DONE!")
    return(FALSE)
  }
}


