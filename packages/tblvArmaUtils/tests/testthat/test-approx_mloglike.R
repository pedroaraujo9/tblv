test_that("input", {
  
  purrr::map(btblv::example_fit, ~{
    expect_no_error({
      .x %>%
        btblv::extract_posterior() %>%
        approx_mloglike(N = 500, cores = 1, seed = 1)
    })
  })
  
  purrr::map(btblv::example_fit, ~{
    expect_no_error({
      .x %>%
        btblv::extract_posterior() %>%
        approx_mloglike(N = 500, cores = 2, seed = 1)
    })
  })
  
})

test_that("output", {
  
  for(cores in c(1, 2)) {
    
    purrr::map(btblv::example_fit, ~{
      mloglike = .x %>%
        btblv::extract_posterior() %>%
        approx_mloglike(N = 400, cores = cores, seed = 1) %>%
        .$aprrox_mloglike
      
      mloglike %>%
        is.na() %>%
        any() %>%
        expect_false()
      
      mloglike %>%
        is.infinite() %>%
        any() %>%
        expect_false()
      
    })
  }
  
})
