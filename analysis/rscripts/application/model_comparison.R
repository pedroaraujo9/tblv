library(tidyverse)
library(btblv)

#### models ####

# btblv
btblv_single = readRDS("analysis/models/btblv-precision=single-K=4.rds")
btblv_specific = readRDS("analysis/models/btblv-precision=specific-K=4.rds")

# bfa
bfa_fit = readRDS("analysis/models/bfa-K=1-10.rds") %>% 
  IMIFA::get_IMIFA_results(Q = 4) 

post_btblv_single = btblv_single %>% btblv::extract_posterior()
post_btblv_specific = btblv_specific %>% btblv::extract_posterior()
post_bfa = bfa_fit %>% btblv::imifa_to_blv(btblv_specific$btblv_data, .)

post = list(
  btblv_single = btblv_single %>% btblv::extract_posterior(),
  btblv_specific = btblv_specific %>% btblv::extract_posterior(),
  bfa = bfa_fit %>% btblv::imifa_to_blv(btblv_specific$btblv_data, .)
)

summ = purrr::map(post, btblv::posterior_summary)
pred = purrr::map(post, btblv::posterior_predict, seed = 1)

fit_metrics = purrr::map2(pred, summ, btblv::check_fit)

fit_metrics %>% 
  purrr::map(~.x$global_metrics) %>%
  purrr::list_simplify() %>%
  dplyr::mutate(model = names(fit_metrics))

fit_metrics %>% 
  purrr::map(~.x$distance_metrics) %>%
  purrr::list_simplify() %>%
  dplyr::mutate(model = names(fit_metrics))
