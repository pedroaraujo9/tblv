test_that("input", {

  data("hmd_data")

  hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1960, 2015, 5)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    ) %>%
    expect_no_error()

  hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1960, 2015, 5)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    ) %>%
    expect_no_warning()


  hmd_data$life_tables_5x1 %>%
    dplyr::filter(year %in% seq(1960, 2000, 1)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    ) %>%
    expect_no_error()

  hmd_data$life_tables_5x1 %>%
    dplyr::filter(year %in% seq(1960, 2000, 1)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    ) %>%
    expect_no_warning()

  hmd_data$life_tables_5x5 %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    ) %>%
    expect_error()

})


test_that("time variable", {

  btblv_data_5 = hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1960, 2015, 5)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    )

  btblv_data_1 = hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1960, 2015, 5)) %>%
    btblv::create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    )

  btblv_data_5$data %>%
    select(ind_num, group_num, time_num) %>%
    arrange(group_num, time_num) %>%
    distinct(group_num, .keep_all = T) %>%
    .$ind_num %>%
    expect_identical(btblv_data_5$data_list_stan$init_index)

  btblv_data_1$data %>%
    select(ind_num, group_num, time_num) %>%
    arrange(group_num, time_num) %>%
    distinct(group_num, .keep_all = T) %>%
    .$ind_num %>%
    expect_identical(btblv_data_1$data_list_stan$init_index)

  (
    btblv_data_5$data_list_stan$init_index %in%
      btblv_data_5$data_list_stan$current_index
  ) %>%
    any() %>%
    expect_false()

  (
    btblv_data_1$data_list_stan$init_index %in%
      btblv_data_1$data_list_stan$current_index
  ) %>%
    any() %>%
    expect_false()

  expect_true(
    all((btblv_data_5$data_list_stan$current_index - btblv_data_5$data_list_stan$past_index) == 1)
  )

  expect_true(
    all((btblv_data_1$data_list_stan$current_index - btblv_data_1$data_list_stan$past_index) == 1)
  )

})
