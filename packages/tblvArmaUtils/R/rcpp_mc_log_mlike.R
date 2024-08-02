#' Approximates the marginal likelihood with Monte Carlo integration
#' for the btblv model.
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
#' @param E_post_sample_list List with the latent effect posterior (theta_{t}) 
#' sample for each time point.
#'
#' @return matrix (iter x time point) with the log of the marginal likelihood 
#' for each time point. It is still necessary to use `exp()` and `sum()`.
#' @export
#'
#' @examples
#' # 
"rcpp_mc_log_mlike" 