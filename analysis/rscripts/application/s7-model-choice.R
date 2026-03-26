library(tidyverse)
library(tblvArmaUtils)
library(btblv)

compute_metrics = function(models_path, precision_type) {
  
  precision_match = paste0("precision=", precision_type)
  models_path = models_path[stringr::str_detect(models_path,  precision_match)]
  
  Ks = models_path %>%
    stringr::str_extract("K=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>%
    as.integer() %>%
    na.omit() %>%
    sort()
  
  metrics_summ = lapply(Ks, function(k){
    model_path = models_path[stringr::str_detect(models_path, paste0("K=", k, ".rds"))]
    print(model_path)
    
    model_fit = readRDS(model_path)
    post_sample = model_fit$btblv_fit %>% 
      extract_posterior(alpha_reference = "mode")
    
    summ = post_sample %>% posterior_summary()
    pred = post_sample %>% posterior_predict(seed = 1)
    
    model_waic = post_sample %>% compute_WAIC() %>% .$waic
    model_bic = post_sample %>% compute_BIC(N = 200000, seed = 1, cores = 6)
    
    data.frame(
      K = k,
      WAIC = model_waic,
      BIC = model_bic,
      log_kappa = summ$posterior_mean$log_kappa %>% mean(),
      RMSE = sqrt(mean((pred$pred_post_summary_df$mean - pred$pred_post_summary_df$y)^2))
    )
    
  }) %>%
    do.call(rbind, .)
  
  return(metrics_summ)
}

#### compute metrics ####
path = "analysis/models"
models = list.files(path)
models_path = paste0(path, "/", models)
models_path = models_path[stringr::str_detect(models_path, "qx-incomplete-precision")]
print(models_path)

metrics_path = paste0("analysis/results/", list.files("analysis/results"))
single_prec_path = "analysis/results/model_choice_metrics_single.rds"

metrics_single_prec = compute_metrics(models_path, "single")
saveRDS(metrics_single_prec, single_prec_path)
metrics_single_prec = readRDS(single_prec_path)
# analysis
metrics_single_prec
metrics_single_prec$BIC %>% plot()
metrics_single_prec$WAIC %>% plot()
metrics_single_prec$log_kappa %>% plot()
metrics_single_prec$RMSE %>% plot()

metrics_single_prec_tidy = metrics_single_prec %>%
  gather(metric, value, -K) %>%
  mutate(metric = factor(metric, levels = c("log_kappa", "BIC", "WAIC", "RMSE")))

metrics_single_prec_tidy %>%
  filter(metric == "BIC") %>%
  arrange(value)

levels(metrics_single_prec_tidy$metric) = c(
  "log_kappa" = latex2exp::TeX("$\\log(\\kappa)$"),
  "BIC" = latex2exp::TeX("BIC$_{m}$"),
  "WAIC" = latex2exp::TeX("WAIC$_{c}$"),
  "RMSE" = "RMSE"
)

metrics_single_prec_tidy %>%
  ggplot(aes(x=K, y=value)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ metric, scales = "free", labeller = label_parsed) +
  labs(x="K", y="Metric value") + 
  scale_x_continuous(breaks = 1:10)

ggsave("analysis/plots/model_choice.pdf", height = 3, width = 5)

metrics_single_prec %>%
  mutate(RMSE = 100*RMSE) %>%
  round(3) %>%
  select(-K) %>%
  xtable::xtable() %>%
  print()


