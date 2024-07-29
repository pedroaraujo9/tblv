#!/bin/bash -l

iter=10; warmup=5; thin=1; chains=3;

for K in $(seq 1 10)
  do 
  Rscript analysis/rscripts/fit_mortality_data.R --args K=$K iter=$iter warmup=$warmup thin=$thin chains=$chains precision=single path_to_save="analysis/models/";
  done