test_that("no input error", {
  data("hmd_data")

  lf = hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1950, 2015, 5)) %>%
    dplyr::filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                                   "New Zealand Non-Maori", "England and Wales (Total Population)",
                                   "England and Wales (Civilian Population)",
                                   "Scotland", "Northern Ireland", "Wales")))
  expect_no_error(
    btblv::create_btblv_data(df = lf,
                             resp_col_name = "mx",
                             item_col_name = "age",
                             group_col_name = "country",
                             time_col_name = "year")
  )

  expect_no_error(
    btblv::create_btblv_data(df = hmd_data$life_tables_5x5,
                             resp_col_name = "mx",
                             item_col_name = "age",
                             group_col_name = "country",
                             time_col_name = "year")
  )

  expect_no_error(
    btblv::create_btblv_data(df = hmd_data$life_tables_5x1,
                             resp_col_name = "mx",
                             item_col_name = "age",
                             group_col_name = "country",
                             time_col_name = "year")
  )

})
