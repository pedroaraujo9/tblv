test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{

    #### default ####
    .x %>%
      extract_posterior() %>%
      expect_no_error()

    .x %>%
      extract_posterior() %>%
      expect_no_warning()

    #### pca ####
    .x %>%
      extract_posterior(alpha_reference = "pca") %>%
      expect_no_error()

    .x %>%
      extract_posterior(alpha_reference = "pca") %>%
      expect_no_warning()

    #### posterior mode ####
    .x %>%
      extract_posterior(alpha_reference = "mode") %>%
      expect_no_error()

    .x %>%
      extract_posterior(alpha_reference = "mode") %>%
      expect_no_warning()

    #### pca and varimax TRUE  ####
    .x %>%
      extract_posterior(alpha_reference = "pca", apply_varimax = TRUE) %>%
      expect_no_error()

    .x %>%
      extract_posterior(alpha_reference = "pca", apply_varimax = TRUE) %>%
      expect_no_warning()

    #### mode and variamx TRUE ####
    .x %>%
      extract_posterior(alpha_reference = "mode", apply_varimax = TRUE) %>%
      expect_no_error()

    .x %>%
      extract_posterior(alpha_reference = "mode", apply_varimax = TRUE) %>%
      expect_no_warning()

    #### custom alpha reference ####
    post = .x %>%
      extract_posterior(alpha_reference = "mode", apply_varimax = TRUE) %>%
      posterior_summary()

    .x %>%
      extract_posterior(alpha_reference = post$posterior_mean$alpha) %>%
      expect_no_error()

    .x %>%
      extract_posterior(alpha_reference = post$posterior_mean$alpha,
                        apply_varimax = TRUE) %>%
      expect_no_error()

    #### non expected input
    .x %>%
      extract_posterior(alpha_reference = "aaaa", apply_varimax = TRUE) %>%
      expect_error()

    .x %>%
      extract_posterior(alpha_reference = c(1, 2, 3)) %>%
      expect_error()

    .x %>%
      extract_posterior(alpha_reference = rbind(c(1, 2, 3), c(1, 2, 3))) %>%
      expect_error()
  })

})
