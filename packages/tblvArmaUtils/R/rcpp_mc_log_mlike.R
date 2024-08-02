#' Approximates the marginal likelihood with Monte Carlo integration
#' for the btblv model
#'
#' Approximates the marginal likelihood with respect to the latent variables 
#' with Monte Carlo integration with importance sampling.
#'
#' @param N integer with Monte Carlo sample size.
#' @param x matrix with observed data.
#' @param alpha numeric matrix (items x latent dimension size) with the 
#' estimated alpha.
#' @param beta numeric matrix (items x 1) with the estimated beta.
#' @param kappa numeric matrix (items x 1) with the estimated kappa.
#' @param phi numeric matrix (groups x 1) with the estimated phi.
#' @param sigma numeric matrix (groups x 1) with the estimated sigma.
#' @param E_post_sample_list List with the latent effect posterior 
#' sample for each time point.
#'
#' @return matrix (iters x 1) with the log of the marginal likelihood sample.
#' @export
#'
#' @examples
#' # 
rcpp_mc_log_mlike <- function(N, 
                              x, 
                              alpha, 
                              beta, 
                              kappa, 
                              phi, 
                              sigma, 
                              E_post_sample_list) {
  .Call(`_tblvArmaUtils_rcpp_mc_log_mlike`, N, x, alpha, beta, kappa, phi, sigma, E_post_sample_list)
}