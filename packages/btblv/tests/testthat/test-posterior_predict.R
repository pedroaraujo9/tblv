test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      posterior_predict(seed = 1) %>%
      expect_no_error()
  })

    purrr::map(example_fit, ~{
      .x %>%
        extract_posterior() %>%
        posterior_predict(seed = 1) %>%
        expect_no_warning()
  })
})

test_that("output", {

  data("example_fit")

  purrr::map(example_fit, ~{
    pred = .x %>%
      extract_posterior() %>%
      posterior_predict(seed = 1)

    # check if the prediction is between (0, 1)
    expect_true(
      all(pred$pred_post_sample < 1) & all(pred$pred_post_sample > 0)
    )

    # check if there is any NA
    pred$pred_post_sample %>%
      is.na() %>%
      any() %>%
      expect_false()

    # check if there is any infinite
    pred$pred_post_sample %>%
      is.infinite() %>%
      any() %>%
      expect_false()

    # check if pred and observed is highly correlated
    cor(pred$pred_post_summary_df$mean, .x$btblv_data$data$y) %>%
      expect_gt(0.8)
  })

})
