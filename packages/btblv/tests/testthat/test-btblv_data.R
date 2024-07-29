test_that("works", {
  data("lf")
  lf = lf %>% filter(year %in% seq(1950, 2015, 5))
  expect_no_error(
    btblv::create_btblv_data(df = lf,
                             resp_col_name = "mx",
                             item_col_name = "age",
                             group_col_name = "country",
                             time_col_name = "year")
  )

})
