test_that("input", {
  
  purrr::map(btblv::example_fit, ~{
    expect_no_error({
      .x %>%
        btblv::extract_posterior() %>%
        approx_mloglike(N = 500, cores = 1)
    })
  })
  
  purrr::map(btblv::example_fit, ~{
    expect_no_error({
      .x %>%
        btblv::extract_posterior() %>%
        approx_mloglike(N = 500, cores = 2)
    })
  })
  
})

test_that("output", {
  purrr::map(btblv::example_fit, ~{
    mloglike = .x %>%
      btblv::extract_posterior() %>%
      approx_mloglike(N = 500, cores = 2) %>%
      .$aprrox_mloglike
    
    expect_false(any(is.na(mloglike)))
    expect_false(any(is.infinite(mloglike)))
  })
})
