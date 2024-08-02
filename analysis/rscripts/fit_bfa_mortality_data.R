library(IMIFA)
library(tidyverse)
library(btblv)

lf = readRDS("analysis/data/data_model.rds")

data = btblv::create_btblv_data(df = lf,
                                resp_col_name = "mx",
                                item_col_name = "age",
                                group_col_name = "country",
                                time_col_name = "year")

log_mx = data$data_list_stan$x %>% log()

bfa_fit = mcmc_IMIFA(
  log_mx, method = "FA", range.Q=1:10,
  mixFA = mixfaControl(
    n.iters = 50000,
    burnin = 20000,
    thinning = 20,
    centering = TRUE,
    scaling = "none"
  ) 
)

saveRDS(bfa_fit, "analysis/models/bfa-K=1-10.rds")

