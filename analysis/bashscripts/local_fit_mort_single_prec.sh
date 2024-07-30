#!/bin/bash -l

iter=10; warmup=5; thin=1; chains=3; max_K=10;

path="analysis/models"
gdrive=TRUE
gdrive_folder_id="1qlT8pJsHhzQF8-XCa7bS-f5fmEwcEIcB"
model="single"

module load R

for K in $(seq 1 $max_K)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path save_gdrive=$gdrive gdrive_folder_id=$gdrive_folder_id; 
  done

wait;


