library(IMIFA)
library(tidyverse)
library(btblv)

model_data = readRDS("analysis/data/btblv_incomplete_data_qx.rds")

logit_qx = model_data$data_list_stan$x %>% logit()

bfa_fit = mcmc_IMIFA(
  logit_qx, 
  method = "FA", 
  range.Q=1:10,
  mixFA = mixfaControl(
    n.iters = 50000,
    burnin = 20000,
    thinning = 20,
    centering = TRUE,
    scaling = "none"
  ) 
)

saveRDS(bfa_fit, "analysis/models/qx-bfa-K=1-10.rds")
bfa_post = bfa_fit %>% IMIFA::get_IMIFA_results()
bfa_post
