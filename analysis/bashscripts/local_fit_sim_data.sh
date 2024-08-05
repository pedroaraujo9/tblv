#!/bin/bash -l

iter=10
warmup=5
thin=1
mc_samples=100
chains=3
precision="single"
config_path="config.yaml" 
model_name_pattern=$1
save_gdrive="FALSE"
data_path=$2
local_save_path="analysis/models/simulation-study"
gdrive_folder_id="1rNyxQWOLd9MjxXkR2vWnin-l2Xvuu65j"

for K in $(seq 1 5); do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$precision config_path=$config_path model_name_pattern=$model_name_pattern save_gdrive=$save_gdrive data_path=$data_path local_save_path=$local_save_path gdrive_folder_id=$gdrive_folder_id mc_samples=$mc_samples; 
done

  

