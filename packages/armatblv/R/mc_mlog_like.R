#' Monte Carlo approximation for the marginal likelihood
#'
#' @param N integer value with the Monte Carlo sample size
#' @param x numeric matrix with the data
#' @param alpha numeric matrix with estimated alpha
#' @param beta numeric matrix (number of items x 1) with estimated beta
#' @param kappa numeric matrix (number of items x 1) with estimated kappa
#' @param phi numeric value with estimated phi
#' @param sigma numeric value with estimated sigma
#' @param E_post_sample_list List where each element is posterior sample of E (iter x Latent dimension)
#'
#' @return log of the monte carlo sample for the likelihood
#' @export
#'
#' @examples
#' #
mc_mlog_like = function(N, x, alpha, beta, kappa, phi, sigma, E_post_sample_list) {
  .Call('_armatblv_mc_mlog_like', PACKAGE = 'armatblv', N, x, alpha, beta, kappa, phi, sigma, E_post_sample_list)
}
