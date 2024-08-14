library(tidyverse)
library(btblv)

imifa_fit = readRDS("analysis/models/bfa-K=1-10.rds")

results = lapply(1:10, function(K){
  btblv_fit_single = readRDS(paste0("analysis/models/btblv-precision=single-K=", K, ".rds"))
  btblv_fit_specific = readRDS(paste0("analysis/models/btblv-precision=specific-K=", K, ".rds"))
  
  # bfa
  imifa_post = imifa_fit %>% IMIFA::get_IMIFA_results(Q = K) 
  post_bfa = imifa_post %>% btblv::imifa_to_blv(btblv_fit_specific$btblv_data, .)
  
  # BTBLV single
  set.seed(1)
  
  post_btblv_single = btblv_fit_single %>%  
    btblv::extract_posterior(alpha_reference = "pca", apply_varimax = FALSE) %>%
    posterior_summary()
  
  ref_alpha = post_btblv_single$posterior_mean$alpha
  
  post_btblv_single = btblv_fit_single %>%  
    btblv::extract_posterior(alpha_reference = ref_alpha, apply_varimax = TRUE)
  
  #btblv specific
  post_btblv_specific = btblv_fit_specific %>%  
    btblv::extract_posterior(alpha_reference = "pca", apply_varimax = FALSE) %>%
    posterior_summary()
  
  ref_alpha = post_btblv_specific$posterior_mean$alpha
  
  post_btblv_specific = btblv_fit_specific %>%  
    btblv::extract_posterior(alpha_reference = ref_alpha, apply_varimax = TRUE)
  
  #### post
  post = list(
    btblv_single = post_btblv_single,
    btblv_specific = post_btblv_specific,
    bfa = post_bfa
  )
  
  summ = purrr::map(post, btblv::posterior_summary)
  pred = purrr::map(post, btblv::posterior_predict, seed = 1)
  
  fit_metrics = purrr::map2(pred, summ, btblv::check_fit)
  
  mu = fit_metrics %>% 
    purrr::map(~.x$global_metrics) %>%
    purrr::list_simplify() %>%
    dplyr::mutate(model = names(fit_metrics)) %>%
    dplyr::mutate(K = K)
  
  distance = fit_metrics %>% 
    purrr::map(~.x$distance_metrics) %>%
    purrr::list_simplify() %>%
    dplyr::mutate(model = names(fit_metrics)) %>%
    dplyr::mutate(K = K)
  
  list(mu = mu, distance = distance)
})

results %>% saveRDS("analysis/results/model_comparison_check_fit.rds")
results = readRDS("analysis/results/model_comparison_check_fit.rds")

mu = purrr::map_df(results, ~.x$mu)
distance = purrr::map_df(results, ~.x$distance)


#### mu RMSE #### 
mu %>% 
  dplyr::select(RMSE, model, K) %>%
  mutate(RMSE = 100*RMSE) %>%
  spread(model, RMSE) 

mu %>% 
  dplyr::select(RMSE, model, K) %>%
  mutate(RMSE = 100*RMSE) %>%
  spread(model, RMSE) %>%
  filter(K %in% c(2, 4, 6))

mu %>%
  ggplot(aes(x=K, y=RMSE, color=model)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10)

#### mu MAPE #### 
mu %>% 
  dplyr::select(MAPE, model, K) %>%
  spread(model, MAPE) 

mu %>% 
  dplyr::select(MAPE, model, K) %>%
  spread(model, MAPE) %>%
  filter(K %in% c(2, 4, 6)) %>%
  as.data.frame() %>%
  round(3)

mu %>%
  ggplot(aes(x=K, y=MAPE, color=model)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10)

#### distance RMSE #### 
distance %>% 
  dplyr::select(RMSE, model, K) %>%
  mutate(RMSE = 100*RMSE) %>%
  spread(model, RMSE) 

distance %>%
  ggplot(aes(x=K, y=RMSE, color=model)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10)

#### distance MAPE #### 
distance %>% 
  dplyr::select(MAPE, model, K) %>%
  spread(model, MAPE) %>%
  as.data.frame() %>%
  round(3)

distance %>%
  ggplot(aes(x=K, y=MAPE, color=model)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10)

#### distance corr #### 
distance %>% 
  dplyr::select(corr, model, K) %>%
  spread(model, corr) 

distance %>% 
  dplyr::select(corr, model, K) %>%
  spread(model, corr) %>%
  filter(K %in% c(2, 4, 6)) %>%
  as.data.frame() %>%
  round(3)


distance %>%
  ggplot(aes(x=K, y=corr, color=model)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10)

#### prediction ####
K = 4
btblv_fit_single = readRDS(paste0("analysis/models/btblv-precision=single-K=", K, ".rds"))
btblv_fit_specific = readRDS(paste0("analysis/models/btblv-precision=specific-K=", K, ".rds"))


imifa_post = imifa_fit %>% IMIFA::get_IMIFA_results(Q = K) 
post_bfa = imifa_post %>% btblv::imifa_to_blv(btblv_fit_specific$btblv_data, .)


single_pred = btblv_fit_single %>% extract_posterior() %>% posterior_predict(seed = 1)
bfa_pred = post_bfa %>% posterior_predict(seed = 1)


avg_preds = single_pred$pred_post_summary_df %>%
  group_by(item, time) %>%
  summarise(BLV = mean(mean), Observed = mean(y)) %>%
  left_join(
    bfa_pred$pred_post_summary_df %>%
      group_by(item, time) %>%
      summarise(BFA = mean(mean)),
    by = c("item", "time")
  )


avg_preds %>%
  filter(item %in% c(0, 20, 45, 100)) %>%
  mutate(item = paste0("Age group: ", item) %>% factor(levels = paste0(paste0("Age group: ", unique(item))))) %>%
  gather(model, avg, -item, -time) %>%
  ggplot(aes(x=time, y=avg, color=model)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ item, scales = "free") + 
  labs(x="Year", y="Average mortality", color="Model")

ggsave("analysis/plots/pred_post.pdf", width = 5.5, height = 2.9)


avg_preds %>%
  filter(!(item %in% c(0, 20, 45, 100))) %>%
  mutate(item = paste0("Age group: ", item) %>% factor(levels = paste0(paste0("Age group: ", unique(item))))) %>%
  gather(model, avg, -item, -time) %>%
  ggplot(aes(x=time, y=avg, color=model)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ item, scales = "free", ncol = 4) + 
  labs(x="Year", y="Average mortality", color="Model")


ggsave("analysis/plots/pred_post_rest.pdf", width = 11, height = 7)
