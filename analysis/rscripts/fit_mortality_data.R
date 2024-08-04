library(btblv)
library(dplyr)
library(stringr)
library(googledrive)

config = yaml::yaml.load_file("config.yaml")

#### parameters from the terminal ####
args = commandArgs(trailingOnly = T)[-1]
args_list = list()

for(i in 1:length(args)) {
  name_value = args[i] %>% stringr::str_split("=", simplify = T) %>% as.character()
  args_list[name_value[1]] = name_value[2] 
}

print(args)

##### data inputs ##### 
data_path = args_list$data_path

##### model inputs ##### 
K = args_list$K %>% as.numeric()
iter = args_list$iter %>% as.numeric()
warmup = args_list$warmup %>% as.numeric()
thin = args_list$thin %>% as.numeric()
chains = args_list$chains %>% as.numeric()
precision = args_list$precision

##### file config inputs ##### 
save_gdrive = args_list$save_gdrive %>% as.logical()

# if the google drive folder is not provided use the one in the config.yaml
gdrive_folder_id = args_list$gdrive_folder_id

if(is.null(gdrive_folder_id)) {
  gdrive_folder_id = config$gdrive$model_folder_id
}

path_to_save = args_list$path_to_save 
model_name_pattern = args_list$model_name_pattern

print(args_list)



