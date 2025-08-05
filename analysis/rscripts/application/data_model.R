library(btblv)
library(tidyverse)

life_tables_5x1 = readRDS("analysis/data/life_tables_5x1.rds")

data_model = life_tables_5x1 %>%
  filter(age <= 85) %>%
  filter(year %in% seq(1960, 2019, 1)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales",
                          "Luxembourg", "Iceland")))

data_model %>%
  btblv::create_btblv_data("mx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_data_mx_2019.rds")


data_model = life_tables_5x1 %>%
  filter(age <= 85) %>%
  filter(year %in% seq(1960, 2010, 1)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales",
                          "Luxembourg", "Iceland")))

data_model %>%
  btblv::create_btblv_data("mx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_data_mx_2010.rds")