test_that("input", {

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
    for(prec in c("single", "specific")){

      fit = fit_btblv(
        btblv_data,
        K = k,
        precision = prec,
        iter = 10,
        warmup = 5,
        thin = 1,
        chains = 2,
        cores = 2,
        seed = 1,
        refresh = 0,
        show_messages = FALSE,
        verbose = FALSE,
        open_progress = FALSE
      ) %>%
        suppressWarnings()

      sm = fit$stan_fit %>% rstan::extract()

      sm %>%
        names() %>%
        expect_contains(c("alpha", "beta", "log_kappa",
                          "theta", "E", "phi", "sigma"))
      sm$E %>%
        dim() %>%
        expect_equal(c(10, 306, k))

      sm$E %>%
        is.na() %>%
        any() %>%
        expect_false()

      if(prec == "single") {

        sm$log_kappa %>%
          dim() %>%
          length() %>%
          expect_equal(1)

      }else if(prec == "specific") {

        sm$log_kappa %>%
          dim() %>%
          length() %>%
          expect_equal(2)
      }
    }

  }

})
