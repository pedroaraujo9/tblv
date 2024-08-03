test_that("input", {
  data("example_fit")

  expect_no_error(
    example_fit$single_K1 %>% extract_posterior() %>% posterior_predict(seed = 1)
  )

  expect_no_error(
    example_fit$single_K2 %>% extract_posterior() %>% posterior_predict(seed = 1)
  )

  expect_no_error(
    example_fit$specific_K1 %>% extract_posterior() %>% posterior_predict(seed = 1)
  )

  expect_no_error(
    example_fit$specific_K2 %>% extract_posterior() %>% posterior_predict(seed = 1)
  )
})

test_that("output", {
  data("example_fit")

  pred_single_K1 = example_fit$single_K1 %>%
    extract_posterior() %>%
    posterior_predict(seed = 1)

  pred_single_K2 = example_fit$single_K2 %>%
    extract_posterior() %>%
    posterior_predict(seed = 1)

  pred_specific_K1 = example_fit$specific_K1 %>%
    extract_posterior() %>%
    posterior_predict(seed = 1)

  pred_specific_K2 = example_fit$specific_K2 %>%
    extract_posterior() %>%
    posterior_predict(seed = 1)

  # K = 1, single prec
  expect_true(
    all(pred_single_K1$pred_post_sample < 1) & all(pred_single_K1$pred_post_sample > 0)
  )

  expect_false(
    any(is.na(pred_single_K1$pred_post_sample))
  )

  expect_gt(
    cor(pred_single_K1$pred_post_summary_df$mean,
        example_fit$single_K1$btblv_data$data$y),
    0.8
  )

  # K = 2, single prec
  expect_true(
    all(pred_single_K2$pred_post_sample < 1) & all(pred_single_K2$pred_post_sample > 0)
  )

  expect_false(
    any(is.na(pred_single_K2$pred_post_sample))
  )

  expect_gt(
    cor(pred_single_K2$pred_post_summary_df$mean,
        example_fit$single_K2$btblv_data$data$y),
    0.8
  )

  # K = 1, specific prec
  expect_true(
    all(pred_specific_K1$pred_post_sample < 1) & all(pred_specific_K1$pred_post_sample > 0)
  )

  expect_false(
    any(is.na(pred_specific_K1$pred_post_sample))
  )

  expect_gt(
    cor(pred_specific_K1$pred_post_summary_df$mean,
        example_fit$specific_K1$btblv_data$data$y),
    0.8
  )

  # K = 2, specific prec
  expect_true(
    all(pred_specific_K2$pred_post_sample < 1) & all(pred_specific_K2$pred_post_sample > 0)
  )

  expect_false(
    any(is.na(pred_specific_K2$pred_post_sample))
  )

  expect_gt(
    cor(pred_specific_K2$pred_post_summary_df$mean,
        example_fit$specific_K2$btblv_data$data$y),
    0.8
  )

})
