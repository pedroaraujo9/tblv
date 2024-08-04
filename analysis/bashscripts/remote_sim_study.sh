#!/bin/bash 

trueKs=(2 4)

for trueK in "${trueKs[@]}"; do
  for repl in $(seq 1 3); do 
    model_name_pattern="trueK=$trueK-repl=$repl"
    data_path="analysis/data/simulation-study/sim_data_trueK=$trueK-replicate=$repl.rds"
    echo $model_name_pattern 
    echo $data_path
    sbash analysis/bashscripts/remote_fit_sim_data.sh "$model_name_pattern" "$data_path"
  done
done