library(dplyr)
library(btblv)

data("lf")

lf = lf %>%
  filter(year %in% seq(1950, 2015, 5)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales")))

data = btblv::create_btblv_data(df = lf,
                                resp_col_name = "mx",
                                item_col_name = "age",
                                group_col_name = "country",
                                time_col_name = "year")

example_fit_single_K2 = btblv_fit(data,
                                  precision = "single",
                                  K = 2,
                                  iter = 100,
                                  warmup = 50,
                                  thin = 1,
                                  chains = 3,
                                  cores = 3,
                                  seed = 1)

usethis::use_data(example_fit_single_K2, overwrite = T)


example_fit_single_K1 = btblv_fit(data,
                                  precision = "single",
                                  K = 1,
                                  iter = 100,
                                  warmup = 50,
                                  thin = 1,
                                  chains = 3,
                                  cores = 3,
                                  seed = 1)

usethis::use_data(example_fit_single_K1, overwrite = T)


example_fit_specific_K2 = btblv_fit(data,
                                    precision = "specific",
                                    K = 2,
                                    iter = 100,
                                    warmup = 50,
                                    thin = 1,
                                    chains = 3,
                                    cores = 3,
                                    seed = 1)

usethis::use_data(example_fit_single_K1, overwrite = T)


example_fit_specific_K1 = btblv_fit(data,
                                    precision = "specific",
                                    K = 1,
                                    iter = 100,
                                    warmup = 50,
                                    thin = 1,
                                    chains = 3,
                                    cores = 3,
                                    seed = 1)

usethis::use_data(example_fit_single_K1, overwrite = T)


