#!/bin/bash -l

# Set the number of nodes

#SBATCH -N 1

# Set the number of tasks/cores per node required 
#SBATCH -n 35

# Set the walltime of the job to 1 hour (format is hh:mm:ss)
#SBATCH -t 100:00:00

# E-mail on begin (b), abort (a) and end (e) of job
#SBATCH --mail-type=ALL

# E-mail address of recipient
#SBATCH --mail-user=pedro.menezesdearaujo@ucdconnect.ie

# Specifies the jobname
#SBATCH --job-name=single-precision
iter=10; warmup=5; thin=1; chains=3; max_K=10;
models_folder="analysis/models"
model_name_pattern="btblv-precision=single-K="
drive_folder_id="1qlT8pJsHhzQF8-XCa7bS-f5fmEwcEIcB"

module load R
module load python

for K in $(seq 1 $max_K)
  do 
  (Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=single path_to_save="analysis/models/"; 
  python3 analysis/pyscripts/upload_fit "${models_folder}/${model_name_pattern}${K}.rds" "${model_name_pattern}${K}.rds" $drive_folder_id; 
  rm "${models_folder}/${model_name_pattern}${K}.rds") & 
  done

wait;


