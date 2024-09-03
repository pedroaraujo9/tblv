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
#SBATCH --job-name=spec-qx

iter=1000
warmup=500 
thin=5
chains=3
precision="specific"
config_path="config.yaml" 
model_name_pattern="prob"
save_gdrive="TRUE"
data_path="analysis/data/data_model.rds"
local_save_path="analysis/models_test"
gdrive_folder_id="1LvmQrUG3P424ZsEVi_LuWfgUHoLmf-LH"
data_type="qx"


module load R

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/application/fit_mortality_data.R --args data_type=$data_type K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$precision config_path=$config_path model_name_pattern=$model_name_pattern save_gdrive=$save_gdrive data_path=$data_path local_save_path=$local_save_path gdrive_folder_id=$gdrive_folder_id &  
  done
