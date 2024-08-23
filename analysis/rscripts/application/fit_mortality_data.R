library(btblv)
library(dplyr)
library(stringr)
library(googledrive)
source("analysis/rscripts/utils.R")

#### parameters from the terminal ####
args = commandArgs(trailingOnly = T)[-1]
args_list = list()

for(i in 1:length(args)) {
  name_value = args[i] %>% 
    stringr::str_split("=", simplify = T, n = 2) %>% 
    as.character()
  
  args_list[name_value[1]] = name_value[2] 
}

print(args_list)

K = args_list$K %>% as.numeric()
data_type = args_list$data_type
iter = args_list$iter %>% as.numeric()
warmup = args_list$warmup %>% as.numeric() 
thin = args_list$thin %>% as.numeric()
chains = args_list$chains %>% as.numeric()
precision = args_list$precision 
config_path = "config.yaml" 
model_name_pattern = args_list$model_name_pattern
save_gdrive = args_list$save_gdrive %>% as.logical()
data_path = args_list$data_path
local_save_path = args_list$local_save_path
gdrive_folder_id = args_list$gdrive_folder_id
mc_samples = args_list$mc_samples %>% as.numeric()

config = yaml::yaml.load_file(config_path)

if(is.null(gdrive_folder_id)) {
  gdrive_folder_id = config$gdrive$model_folder_id
}

if(is.null(data_type) | data_type == "") {
  data_type = "mx"
}

save_fit_btblv(
  K = K, 
  data_type = data_type,
  iter = iter, 
  mc_samples = mc_samples,
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
  gdrive_folder_id = gdrive_folder_id
)

