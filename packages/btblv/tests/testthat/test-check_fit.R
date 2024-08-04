test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    expect_no_error(
      {
        post_pred = .x %>%
          extract_posterior() %>%
          posterior_predict(seed = 1)

        post_summ = .x %>%
          extract_posterior() %>%
          posterior_summary()

        check_fit(post_pred, post_summ)
      }
    )
  })
})
