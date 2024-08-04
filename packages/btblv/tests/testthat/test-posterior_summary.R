test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      posterior_summary() %>%
      expect_no_error()

    .x %>%
      extract_posterior() %>%
      posterior_summary() %>%
      expect_no_warning()
  })
})
