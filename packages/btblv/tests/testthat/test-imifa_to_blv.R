test_that("input", {
  imifa_result = readRDS("../imifa_result.rds")

  #### check inputs ####
  purrr::map(imifa_result$fits, ~{
    imifa_to_blv(imifa_result$btblv_data, .x) %>%
      expect_no_error()
  })

  purrr::map(imifa_result$fits, ~{
    imifa_to_blv(imifa_result$btblv_data, .x) %>%
      expect_no_warning()
  })

  #### check other methods
  imifa_res = purrr::map(imifa_result$fits, ~{
    imifa_to_blv(imifa_result$btblv_data, .x)
  })

  purrr::map(imifa_res, ~{
    summ = .x %>% posterior_summary()
    pred = .x %>% posterior_predict(seed = 1)
    check_fit(pred, summ)
  })
})
