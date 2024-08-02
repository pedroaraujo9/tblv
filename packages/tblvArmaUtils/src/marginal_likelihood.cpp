#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>

static double const log2pi = std::log(2.0 * M_PI);

void inplace_tri_mat_mult(arma::rowvec &x, arma::mat const &trimat){
  arma::uword const n = trimat.n_cols;
  
  for(unsigned j = n; j-- > 0;){
    double tmp(0.);
    for(unsigned i = 0; i <= j; ++i)
      tmp += trimat.at(i, j) * x[i];
    x[j] = tmp;
  }
} 

arma::vec dmvnrm_arma_fast(arma::mat const &x,  
                           arma::rowvec const &mean,  
                           arma::mat const &sigma, 
                           bool const logd = false) { 
  using arma::uword;
  uword const n = x.n_rows, 
    xdim = x.n_cols;
  arma::vec out(n);
  arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
  double const rootisum = arma::sum(log(rooti.diag())), 
    constants = -(double)xdim/2.0 * log2pi, 
    other_terms = rootisum + constants;
  
  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z = (x.row(i) - mean);
    inplace_tri_mat_mult(z, rooti);
    out(i) = other_terms - 0.5 * arma::dot(z, z);     
  }   
  
  if (logd)
    return out;
  return exp(out);
} 

arma::mat expand_vec(arma::vec x, int n) {
  arma::vec one_n(n, arma::fill::ones);
  arma::mat x_expand = kron(one_n, x.t());
  return x_expand;
} 

arma::vec calc_log_like_sample(arma::vec x,
                               arma::mat theta_sample, 
                               arma::mat alpha,
                               arma::mat beta, 
                               arma::mat kappa) { 
  
  int n = theta_sample.n_rows;
  int J = x.n_elem;
  
  arma::vec one_J(J, arma::fill::ones);
  
  arma::mat x_expand = expand_vec(x, n);
  arma::mat beta_expand = expand_vec(beta, n);
  arma::mat kappa_expand = expand_vec(kappa, n);
  
  arma::mat mu = 1.0/(1.0 + exp(-(theta_sample*alpha.t() + beta_expand)));
  arma::vec ll = (lgamma(kappa_expand) - arma::lgamma(mu % kappa_expand) - arma::lgamma((1.0-mu) % kappa_expand) + (mu % kappa_expand - 1.0) % log(x_expand) + ((1.0-mu) % kappa_expand - 1.0) % log(1.0-x_expand))*one_J;
  return ll;
}   

// [[Rcpp::export]]
arma::vec rcpp_mc_log_mlike(int N,
                            arma::mat x, 
                            arma::mat alpha, 
                            arma::mat beta, 
                            arma::mat kappa,
                            double phi, 
                            double sigma, 
                            Rcpp::List E_post_sample_list) {
  int K = alpha.n_cols;
  int T = x.n_rows;
  
  arma::mat I = arma::diagmat(arma::vec(K, arma::fill::ones));
  arma::rowvec zero_vec = arma::rowvec(K, arma::fill::zeros);
  arma::mat theta_ant;
  arma::mat theta;
  arma::mat log_prior;
  arma::mat log_prop;
  arma::mat log_like;
  arma::mat E_prop;
  
  arma::mat E_post_sample = E_post_sample_list[0];
  arma::vec mu_prop = mean(E_post_sample, 0).t();
  arma::mat S_prop = cov(E_post_sample);
  
  E_prop = arma::mvnrnd(mu_prop, S_prop, N).t();
  theta = E_prop*sigma/sqrt(1-pow(phi, 2));
  
  log_like = calc_log_like_sample(x.row(0).t(), theta, alpha, beta, kappa);
  log_prior = dmvnrm_arma_fast(E_prop, zero_vec, I, true);
  log_prop = dmvnrm_arma_fast(E_prop, mu_prop.t(), S_prop, true);
  
  theta_ant = theta;
  
  for(int i=1; i<T; i++) {
    arma::mat E_post_sample = E_post_sample_list[i];
    arma::vec mu_prop = mean(E_post_sample, 0).t();
    arma::mat S_prop = cov(E_post_sample);
    
    E_prop = arma::mvnrnd(mu_prop, S_prop, N).t();
    arma::mat theta = theta_ant*phi + sigma*E_prop;
    
    log_like += calc_log_like_sample(x.row(i).t(), theta, alpha, beta, kappa);
    log_prior += dmvnrm_arma_fast(E_prop, zero_vec, I, true);
    log_prop += dmvnrm_arma_fast(E_prop, mu_prop.t(), S_prop, true);
    
    theta_ant = theta; 
    
  }   
  
  return log_like + log_prior + log_prop;
}   


// [[Rcpp::export]]
double LSE(arma::vec x) {
  return max(x) + log(sum(exp(x - max(x))));
}  
