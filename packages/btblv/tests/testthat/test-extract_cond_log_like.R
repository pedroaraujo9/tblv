test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      extract_cond_log_like() %>%
      expect_no_error()
  })
})


test_that("outputs", {

  data("example_fit")

  purrr::map(example_fit, ~{
    cond_log_like = .x %>%
      extract_posterior() %>%
      extract_cond_log_like() %>%
      expect_no_error()

    cond_log_like = .x %>%
      extract_posterior() %>%
      extract_cond_log_like() %>%
      expect_no_warning()

    # check if it is a matrix
    cond_log_like %>%
      is.matrix() %>%
      expect_true()

    # check if there is any NA
    cond_log_like %>%
      is.na() %>%
      any() %>%
      expect_false()

    # check if there any Inf
    cond_log_like %>%
      is.infinite() %>%
      any() %>%
      expect_false()
  })
})
