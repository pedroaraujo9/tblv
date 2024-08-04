library(btblv)
library(dplyr)
library(stringr)
library(googledrive)

#### parameters from the terminal ####
args = commandArgs(trailingOnly = T)[-1]
args_list = list()

for(i in 1:length(args)) {
  name_value = args[i] %>% stringr::str_split("=", simplify = T) %>% as.character()
  args_list[name_value[1]] = name_value[2] 
}

print(args)
print(args_list)

K = args_list$K %>% as.numeric()
iter = args_list$iter %>% as.numeric()
warmup = args_list$warmup %>% as.numeric()
chains = args_list$chains %>% as.numeric()
thin = args_list$thin %>% as.numeric()
precision = args_list$precision 

path_to_save = args_list$path_to_save 
save_gdrive = args_list$save_gdrive %>% as.logical()
gdrive_folder_id = args_list$gdrive_folder_id

#### check if the model was already fitted ####
model_name = paste0(
  "btblv-precision=", precision, "-",
  "K=", K, ".rds"
)

print(model_name)

#### fit the model in case the model has not been fitted ####
models_saved = readLines(paste0(path_to_save, "/models-saved-list.txt"))
print(models_saved)

if(!(model_name %in% models_saved)) {
  #### data ####
  lf = readRDS("analysis/data/data_model.rds")
  
  data = btblv::create_btblv_data(
    df = lf,
    resp_col_name = "mx",
    item_col_name = "age",
    group_col_name = "country",
    time_col_name = "year"
  )
  
  fit = fit_btblv(
    data,
    precision = precision,
    K = K,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    cores = chains,
    seed = 1
  )
  
  saveRDS(fit, paste0(path_to_save, "/",model_name))
  
  if(save_gdrive == TRUE) {
    drive_deauth()
    drive_auth_configure(path = "credentials.json")
    drive_auth()
    
    drive_upload(media = paste0(path_to_save, "/",model_name), 
                 path = as_id(gdrive_folder_id),
                 overwrite = TRUE)
    
    file.remove(paste0(path_to_save, "/",model_name))
  }
  
  # updating list with models saved
  c(model_name, models_saved) %>%
    writeLines(paste0(path_to_save, "/models-saved-list.txt"))
  
}else{
  print("Fit already exists")
}

