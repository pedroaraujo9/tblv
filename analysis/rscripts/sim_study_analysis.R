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

fit$metrics$bic


post_summ = fit$btblv_fit %>% 
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

plot(sim_data$true_parameters$E, post_summ$posterior_mean$E)
abline(a=0, b=1)




