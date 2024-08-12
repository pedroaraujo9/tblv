library(dplyr)
library(btblv)

data("hmd_data")

lf = hmd_data$life_tables_5x5 %>%
  filter(year %in% seq(1950, 2015, 5)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales")))

iter = 50
warmup = iter/2
thin = 1
chains = 2
cores = 2
seed = 1

data = btblv::create_btblv_data(
  df = lf,
  resp_col_name = "mx",
  item_col_name = "age",
  group_col_name = "country",
  time_col_name = "year"
)

example_fit_single_K2 = fit_btblv(
  data,
  precision = "single",
  K = 2,
  iter = iter,
  warmup = warmup,
  thin = thin,
  chains = chains,
  cores = cores,
  seed = seed
)


example_fit_single_K1 = fit_btblv(
  data,
  precision = "single",
  K = 1,
  iter = iter,
  warmup = warmup,
  thin = thin,
  chains = chains,
  cores = cores,
  seed = seed
)

example_fit_specific_K2 = fit_btblv(
  data,
  precision = "specific",
  K = 2,
  iter = iter,
  warmup = warmup,
  thin = thin,
  chains = chains,
  cores = cores,
  seed = seed
)

example_fit_specific_K1 = fit_btblv(
  data,
  precision = "specific",
  K = 1,
  iter = iter,
  warmup = warmup,
  thin = thin,
  chains = chains,
  cores = cores,
  seed = seed
)

example_fit = list(
  "single_K1" = example_fit_single_K1,
  "single_K2" = example_fit_single_K2,
  "specific_K1" = example_fit_specific_K1,
  "specific_K2" = example_fit_specific_K2
)

usethis::use_data(example_fit, overwrite = T, )

#### bfa fit for the data use to test functions ####
log_mx = data$data_list_stan$x %>% log()

bfa_fit = IMIFA::mcmc_IMIFA(
  log_mx, method = "FA", range.Q=1:10,
  mixFA = mixfaControl(
    n.iters = 50000,
    burnin = 20000,
    thinning = 20,
    centering = TRUE,
    scaling = "none"
  )
)

imifa_result = list()

imifa_result = list(
  imifa_result_K1 = IMIFA::get_IMIFA_results(imifa_fit, Q = 1),
  imifa_result_K2 = IMIFA::get_IMIFA_results(imifa_fit, Q = 2),
  btblv_data = example_fit$single_K1$btblv_data
)

saveRDS(imifa_result, "inst/imifa_result.rds")







