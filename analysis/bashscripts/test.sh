#!/bin/bash 

trueK=2
repl=3

model_name_pattern="trueK=$trueK-repl=$repl"
data_path="analysis/data/simulation-study/sim_data_trueK=$trueK-replicate=$repl.rds"
sbatch analysis/bashscripts/remote_fit_sim_data.sh "$model_name_pattern" "$data_path"