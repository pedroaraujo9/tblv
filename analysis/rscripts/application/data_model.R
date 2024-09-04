library(btblv)

data_model = hmd_data$life_tables_5x5 %>%
  filter(year %in% seq(1950, 2015, 5)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales")))

saveRDS(data_model, "analysis/data/data_model.rds")

data_model %>%
  btblv::create_btblv_data("mx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_data_mx.rds")

data_model %>%
  filter(age < 110) %>%
  btblv::create_btblv_data("qx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_data_qx.rds")


