test_that("input", {

  # data to test
  btblv_data = hmd_data$life_tables_5x5 %>%
    dplyr::filter(year %in% seq(1980, 2015, 5)) %>%
    dplyr::filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                                   "New Zealand Non-Maori", "England and Wales (Total Population)",
                                   "England and Wales (Civilian Population)",
                                   "Scotland", "Northern Ireland", "Wales"))) %>%
    create_btblv_data(
      resp_col_name = "mx",
      item_col_name = "age",
      group_col_name = "country",
      time_col_name = "year"
    )

  for(k in c(1, 3)) {
    expect_equal(
      {
        fit = btblv_fit(
          btblv_data,
          K = k,
          precision = "single",
          iter = 10,
          warmup = 5,
          thin = 1,
          chains = 3,
          cores = 3,
          seed = 1,
          refresh = 0,
          show_messages = FALSE,
          verbose = FALSE,
          open_progress = FALSE
        )

        sm = fit$stan_fit %>% rstan::extract(pars="E")
        sm$E %>% dim()


      }, expected = c(15, 306, k)
    )

    expect_equal(
      {
        fit = btblv_fit(
          btblv_data,
          K = k,
          precision = "specific",
          iter = 10,
          warmup = 5,
          thin = 1,
          chains = 3,
          cores = 3,
          seed = 1,
          refresh = 0,
          show_messages = FALSE,
          verbose = FALSE,
          open_progress = FALSE
        )

        sm = fit$stan_fit %>% rstan::extract(pars="E")
        sm$E %>% dim()

      }, expected = c(15, 306, k)
    )

  }

})
