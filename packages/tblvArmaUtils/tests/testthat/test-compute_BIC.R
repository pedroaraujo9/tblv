test_that("input", {
  purrr::map(btblv::example_fit, ~{
    expect_no_error(
      .x %>%
        btblv::extract_posterior() %>%
        compute_BIC(N = 500, cores = 1, seed = 1)
    )
  })
  
  purrr::map(btblv::example_fit, ~{
    expect_no_error(
      .x %>%
        btblv::extract_posterior() %>%
        compute_BIC(N = 500, cores = 2, seed = 1)
    )
  })
})
