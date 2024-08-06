library(tidyverse)

sim_models_path = "analysis/models/simulation-study/"
models_path = list.files(sim_models_path)

trueK = models_path %>%
  str_extract("-trueK=\\d{1,10}") %>%
  str_extract("\\d{1,10}") %>%
  as.integer()

K = models_path %>%
  str_extract("-K=\\d{1,10}") %>%
  str_extract("\\d{1,10}") %>%
  as.integer()

repl = models_path %>%
  str_extract("-repl=\\d{1,10}") %>%
  str_extract("\\d{1,10}") %>% 
  as.integer()

models_path = models_path[trueK == K]
models_path

model_path = models_path[1]

print(model_path)

fit = readRDS(file.path(sim_models_path, "btblv-trueK=2-repl=1-precision=single-K=2.rds"))
sim_data = readRDS("analysis/data/sim_data_2.rds")

fit = sim_data$sim_data_list[[1]] %>%
  create_btblv_data("mx", "age", "country", "year") %>%
  fit_btblv(K = 2, iter=50, warmup = 25, 
            thin = 1, chains = 2, cores = 2, seed = 1)

post_summ = fit$btblv_fit %>% 
  extract_posterior() %>%
  posterior_summary()

post_summ = fit %>% 
  extract_posterior() %>%
  posterior_summary()

plot(sim_data$true_parameters$sigma, post_summ$posterior_mean$sigma)
abline(a=0, b=1)

plot(sim_data$true_parameters$phi, post_summ$posterior_mean$phi)
abline(a=0, b=1)

plot(sim_data$true_parameters$beta, post_summ$posterior_mean$beta)
abline(a=0, b=1)

plot(sim_data$true_parameters$alpha, post_summ$posterior_mean$alpha)
abline(a=0, b=1)

plot(sim_data$true_parameters$alpha, post_summ$posterior_mean$alpha)
abline(a=0, b=1)


plot(sim_data$true_parameters$E, post_summ$posterior_mean$E)
abline(a=0, b=1)

sim_data$true_parameters$E %>% plot()

post = fit$btblv_fit %>% 
  extract_posterior()

summ = post %>% posterior_summary()

plot(post$post_sample_array$rot_E[,,1] %>% colMeans(), 
     sim_data$true_parameters$E[, 1])

true_fit = readRDS("analysis/models/btblv-precision=single-K=2.rds")
true_post = true_fit %>% extract_posterior()
true_mean = true_post %>% posterior_summary()

theta1 = true_post$post_sample_array$rot_theta[,,1] %>% colMeans()
theta2 = true_post$post_sample_array$rot_theta[,,2] %>% colMeans()
theta = cbind(theta1, theta2)

plot(theta %>% sort(), summ$posterior_mean$theta %>% sort())
abline(a=0, b=1)



