#!/bin/bash -l

iter=10000; warmup=5000; thin=10; chains=3;

path="analysis/models"
gdrive="TRUE"
gdrive_folder_id="1qlT8pJsHhzQF8-XCa7bS-f5fmEwcEIcB"
model="single"

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path save_gdrive=$gdrive gdrive_folder_id=$gdrive_folder_id; 
  done

wait;


