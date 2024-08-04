test_that("input", {

  data("example_fit")

  purrr::map(example_fit, ~{
    .x %>%
      extract_posterior() %>%
      simulate_data(seed = 1, replicates = 3) %>%
      expect_no_error()
  })

  purrr::map(example_fit, ~{
    sim_data = .x %>%
      extract_posterior() %>%
      simulate_data(seed = 1, replicates = 3)

    cor(sim_data$sim_data_list[[1]]$mx, sim_data$btblv_data$df$mx) %>%
      expect_gt(0.9)

    cor(sim_data$sim_data_list[[2]]$mx, sim_data$btblv_data$df$mx) %>%
      expect_gt(0.9)
  })

})
