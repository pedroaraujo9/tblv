#!/bin/bash -l

iter=10000
warmup=5000 
thin=10
chains=3
precision="single"
config_path="config.yaml" 
model_name_pattern=""
save_gdrive="TRUE"
data_path="analysis/data/data_model.rds"
local_save_path="analysis/models_test"
gdrive_folder_id="1LvmQrUG3P424ZsEVi_LuWfgUHoLmf-LH"

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$precision config_path=$config_path model_name_pattern=$model_name_pattern save_gdrive=$save_gdrive data_path=$data_path local_save_path=$local_save_path gdrive_folder_id=$gdrive_folder_id; 
  done

