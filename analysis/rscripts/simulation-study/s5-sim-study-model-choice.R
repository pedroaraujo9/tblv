library(tidyverse)
library(latex2exp)

sim_study_path = "analysis/models/simulation-study"
models = list.files(sim_study_path)
models = models[str_detect(models, "btblv-trueK")]
models = file.path(sim_study_path, models)
models

sim_study_metrics = purrr:::map_df(models, ~{
  
  print(.x)
  
  trueK = .x %>% 
    stringr::str_extract("trueK=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  repl = .x %>% 
    stringr::str_extract("repl=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  fit = readRDS(.x)
  
  post_summ = fit$btblv_fit %>% 
    btblv::extract_posterior() %>% 
    btblv::posterior_summary()
  
  log_kappa = post_summ$posterior_mean$log_kappa
  
  data.frame(
    MCMBIC = fit$metrics$bic, 
    WAIC = fit$metrics$waic$waic, 
    log_kappa = log_kappa,
    repl = repl,
    trueK = trueK, 
    K = fit$btblv_fit$btblv_data$data_list_stan$K
  )
}) %>% tibble::as_tibble()

sim_study_metrics %>% saveRDS("analysis/results/sim_study_metrics.rds")
sim_study_metrics = readRDS("analysis/results/sim_study_metrics.rds")

sim_study_metrics_tidy = sim_study_metrics %>%
  gather(metric, value, -repl, -trueK, -K) %>%
  mutate(metric = factor(metric, levels = c("MCMBIC",
                                            "WAIC", 
                                            "log_kappa")), 
         trueK = factor(trueK)) %>%
  as_tibble()

# selected model 
sim_study_metrics_tidy %>%
  group_by(repl, metric, trueK) %>%
  summarise(selected_K = K[which.min(value)]) %>%
  group_by(metric, trueK, selected_K) %>%
  summarise(n = n())

sim_study_metrics_tidy %>%
  filter(metric == "log_kappa") %>%
  group_by(repl, metric, trueK) %>%
  summarise(selected_K = K[which.max(value)]) %>%
  group_by(metric, trueK, selected_K) %>%
  summarise(n = n())

# visualizing 
levels(sim_study_metrics_tidy$metric) = c(
  "MCMBIC" = TeX("Metric: BIC$_{m}$"),
  "WAIC" = TeX("Metric: WAIC$_{c}$"),
  "log_kappa" = TeX("Metric: $\\log(\\kappa)$")
)

levels(sim_study_metrics_tidy$trueK) = c(
  "True K = 2" = latex2exp::TeX("True $K = 2$"),
  "True K = 4" = latex2exp::TeX("True $K = 4$")
)


sim_study_metrics_tidy %>%
  ggplot(aes(x=K, y=value, group=repl)) + 
  geom_line(alpha=0.4) + 
  geom_point(alpha=0.4) + 
  facet_wrap(trueK ~ metric, scales = "free", labeller = label_parsed) + 
  labs(x="K", y="Metric value")
ggsave("analysis/plots/simulation-study/sim_study_model_choice.pdf", width = 7.5, height = 4)

sim_study_metrics_tidy %>%
  ggplot(aes(x=factor(K), y=value)) + 
  geom_boxplot() + 
  geom_jitter(alpha=0.3, size = 1) + 
  facet_wrap(trueK ~ metric, scales = "free", labeller = label_parsed) + 
  labs(x="K", y="Metric value") + 
  theme(text = element_text(size = 15))

ggsave("analysis/plots/simulation-study/sim_study_model_choice.pdf", width = 9, height = 5)


