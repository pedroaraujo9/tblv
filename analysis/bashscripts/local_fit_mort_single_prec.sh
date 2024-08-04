#!/bin/bash -l

K = args_list$K %>% as.numeric()
iter = args_list$iter %>% as.numeric()
warmup = args_list$warmup %>% as.numeric()
chains = args_list$chains %>% as.numeric()
thin = args_list$thin %>% as.numeric()
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


iter=10000
warmup=5000 
thin=10
chains=3
precision="single"

save_gdrive="TRUE"
gdrive_folder_id="1LvmQrUG3P424ZsEVi_LuWfgUHoLmf-LH"
data_path="analysis/data/data_model.rds"
path_to_save="analysis/models"

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args data_path=$data_path K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path_to_save save_gdrive=$save_gdrive;  
  done


