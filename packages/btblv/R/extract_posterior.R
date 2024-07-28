#' Title
#'
#' @param btblv_fit
#'
#' @return
#' @export
#'
#' @examples
extract_posterior = function(btblv_fit) {

  stan_fit = btblv_fit$stan_fit
  data = btblv_fit$btblv_data

  post_sample = stan_fit %>% rstan::extract()
  post_sample_chains = stan_fit %>% rstan::extract(permuted = FALSE)

  # rotate params
  eigen_decomp = btblv_data$data_list_stan$x %>% cor() %>% eigen()
  ref_matrix = eigen_decomp$vectors[, 1:K]
  rotation_list = .get_rotation(post_sample$alpha, ref_matrix)
  post_sample$rot_alpha = .apply_rotation(post_sample$alpha, rotation_list)
  post_sample$rot_theta = .apply_rotation(post_sample$theta, rotation_list)

  out = list(
    post_sample_array = post_sample,
    post_sample_chains = post_sample_chains
  )

  class(out) = "btblv_posterior"
}
