#!/bin/bash -l

# Set the number of nodes

#SBATCH -N 1

# Set the number of tasks/cores per node required 
#SBATCH -n 30

# Set the walltime of the job to 1 hour (format is hh:mm:ss)
#SBATCH -t 100:00:00

# E-mail on begin (b), abort (a) and end (e) of job
#SBATCH --mail-type=ALL

# E-mail address of recipient
#SBATCH --mail-user=

# Specifies the jobname
#SBATCH --job-name=single-precision

#!/bin/bash -l

iter=4000
warmup=2000
thin=5
chains=3
mc_samples=100000
precision="single"
config_path="config.yaml" 
model_name_pattern=$1
save_gdrive="TRUE"
data_path=$2
local_save_path="analysis/models/simulation-study"
gdrive_folder_id="1rNyxQWOLd9MjxXkR2vWnin-l2Xvuu65j"

module load R

for K in $(seq 1 6); do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=$precision config_path=$config_path model_name_pattern=$model_name_pattern save_gdrive=$save_gdrive data_path=$data_path local_save_path=$local_save_path gdrive_folder_id=$gdrive_folder_id mc_samples=$mc_samples & 
done
wait;


