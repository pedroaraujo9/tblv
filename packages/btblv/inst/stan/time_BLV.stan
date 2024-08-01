data {
  int<lower=0> n; // number of individuals
  int<lower=0> J; // number of items
  int<lower=0> N; // total number of observations n * J
  int<lower=0> K; // latent effect dimension
  matrix<lower=0, upper=1>[n, J] x; // n * J data matrix
  int Ng; // number of groups
  int init_index[Ng]; // init point index for each group

  int past_index[n-Ng]; // past point index
  int current_index[n-Ng]; // current point index
  int group_id[n-Ng]; // group id
}

parameters {
  matrix[n, K] E; // latent effects
  matrix[J, K] alpha;
  vector[J] beta;
  vector[J-1] delta_raw;
  real baseline_delta;
  vector<lower=-1, upper=1>[Ng] phi;
  vector[Ng] log_sigma;
}

transformed parameters {
  matrix[n, K] theta;
  matrix[n, J] beta_expand;
  beta_expand = rep_matrix(beta, n)';
  vector[Ng] sigma = exp(log_sigma);

  vector[J] delta = append_row(delta_raw, -sum(delta_raw));
  vector[J] log_kappa = baseline_delta + delta;

  vector<lower=0>[J] kappa = exp(log_kappa);

  // prior for the init point
  for(i in 1:Ng) {
    theta[init_index[i]] = (sigma[i] / sqrt(1-phi[i]^2)) * E[init_index[i]];
  }

  // AR(1) prior for the other ones
  for(i in 1:(n-Ng)) {
    theta[current_index[i]] = phi[group_id[i]] * theta[past_index[i]] +
                              sigma[group_id[i]]*E[current_index[i]];
  }
}

model {
  beta ~ normal(0, 10);
  delta_raw ~ normal(0, 10);
  baseline_delta ~ normal(0, 10);

  for(i in 1:Ng) {
    log_sigma[i] ~ normal(0, 1);
    phi[i] ~ uniform(-1, 1);
  }

  for(k in 1:K) {
    alpha[, k] ~ normal(0, 1);
  }

  for(i in 1:n) {
    E[i] ~ std_normal();
  }

  matrix[n, J] mu = inv_logit(beta_expand + (theta*(alpha')));

  for(j in 1:J) {
    x[, j] ~ beta_proportion(mu[, j], kappa[j]);
  }
}
