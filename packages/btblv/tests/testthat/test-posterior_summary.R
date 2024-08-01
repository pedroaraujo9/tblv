test_that("input", {
  data("example_fit")

  expect_no_error(
    example_fit$single_K1 %>% extract_posterior() %>% posterior_summary()
  )

  expect_no_error(
    example_fit$single_K2 %>% extract_posterior() %>% posterior_summary()
  )

  expect_no_error(
    example_fit$specific_K1 %>% extract_posterior() %>% posterior_summary()
  )

  expect_no_error(
    example_fit$specific_K2 %>% extract_posterior() %>% posterior_summary()
  )
})
