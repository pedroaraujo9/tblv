#!/bin/bash -l

iter=10000
warmup=5000 
thin=10
chains=3
model="single"

data_path="analysis/data/data_model.rds"
path_to_save="analysis/models"
save_gdrive="TRUE"

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args data_path=$data_path K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path_to_save save_gdrive=$save_gdrive;  
  done


