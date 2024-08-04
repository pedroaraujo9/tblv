test_that("inputs", {

  # test init values generator
  purrr::map(example_fit, ~{
    .generate_init_values(
      btblv_data = .x$btblv_data,
      K = .x$btblv_data$data_list_stan$K,
      chains = 2,
      precision = .x$precision,
      seed = 1
    ) %>%
      expect_no_error()

    .generate_init_values(
      btblv_data = .x$btblv_data,
      K = .x$btblv_data$data_list_stan$K,
      chains = 2,
      precision = .x$precision,
      seed = 1
    ) %>%
      expect_no_warning()
  })

  # posterior mean calculation
  array1 = array(rnorm(1000, mean = 10, sd = 0.5), dim = c(1000))
  array2 = array(rnorm(1000*5, mean = 10, sd = 0.5), dim = c(1000, 5))
  array3 = array(rnorm(1000*5*3, mean = 10, sd = 0.5), dim = c(1000, 5, 3))

  purrr::map(list(array1, array2, array3), ~{

    .x %>% .compute_posterior_mean() %>% expect_no_error()
    .x %>% .compute_posterior_mean() %>% expect_no_warning()

    .x %>% .get_summary_df() %>% expect_no_error()
    .x %>% .get_summary_df() %>% expect_no_warning()

  })

})

