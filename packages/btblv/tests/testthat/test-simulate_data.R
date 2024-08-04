test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      simulate_data(seed = 1, replicates = 3) %>%
      expect_no_error()
  })

})
