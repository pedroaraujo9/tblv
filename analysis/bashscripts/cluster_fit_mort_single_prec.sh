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

iter=10000;warmup=5000;thin=10; chains=3;

path="analysis/results"
gdrive=TRUE
gdrive_folder_id="1qlT8pJsHhzQF8-XCa7bS-f5fmEwcEIcB"
model=single

module load R
module load python

for K in $(seq 1 10)
  do 
  Rscript rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$model path_to_save=$path save_gdrive=$gdrive gdrive_folder_id=$gdrive_folder_id & 
  done

wait;
