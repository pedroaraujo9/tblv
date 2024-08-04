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

iter=10000
warmup=5000 
thin=10
chains=3
model="specific"

data_path="analysis/data/data_model.rds"
path_to_save="analysis/models"
save_gdrive="TRUE"

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args data_path=$data_path K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path_to_save save_gdrive=$save_gdrive &  
  done

wait;
