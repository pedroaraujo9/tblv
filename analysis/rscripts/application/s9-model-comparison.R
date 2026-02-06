library(tidyverse)
library(btblv)

imifa_fit = readRDS("analysis/models/qx-bfa-K=1-10.rds")

results = lapply(1:10, function(K){
  
  print(K)
  
  btblv_fit_single = readRDS(
    paste0("analysis/models/btblv-qx-incomplete-precision=single-K=", K, ".rds")
  )$btblv_fit
  
  # bfa
  imifa_post = imifa_fit %>% IMIFA::get_IMIFA_results(Q = K) 
  post_bfa = imifa_post %>% imifa_to_blv(btblv_fit_single$btblv_data, ., trans_func = logit)
  
  # BTBLV single
  set.seed(1)
  
  post_btblv_single = btblv_fit_single %>%  
    btblv::extract_posterior(alpha_reference = "pca", apply_varimax = FALSE) %>%
    posterior_summary()
  
  ref_alpha = post_btblv_single$posterior_mean$alpha
  
  post_btblv_single = btblv_fit_single %>%  
    btblv::extract_posterior(alpha_reference = ref_alpha, apply_varimax = TRUE)
  
  
  #### post
  post = list(
    btblv_single = post_btblv_single,
    bfa = post_bfa
  )
  
  summ = purrr::map(post, btblv::posterior_summary)
  pred = purrr::map(post, btblv::posterior_predict, seed = 1, inv_trans_func = inv_logit)
  
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

results %>% saveRDS("analysis/results/qx-model_comparison_check_fit.rds")

results = readRDS("analysis/results/qx-model_comparison_check_fit.rds")

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
  spread(model, RMSE) %>%
  filter(K %in% c(2, 4, 6))

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
  round(3) %>%
  filter(K %in% c(2, 4, 6))

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
K = 6
btblv_fit_single = readRDS(
  paste0("analysis/models/btblv-qx-incomplete-precision=single-K=", K, ".rds")
)

imifa_fit %>% IMIFA::get_IMIFA_results()

imifa_post = imifa_fit %>% IMIFA::get_IMIFA_results(Q = K) 
post_bfa = btblv::imifa_to_blv(
  btblv_data = btblv_fit_single$btblv_fit$btblv_data,
  imifa_result =  imifa_post, 
  trans_func = logit
)

single_pred = btblv_fit_single$btblv_fit %>% 
  extract_posterior() %>% 
  posterior_predict(seed = 1)

bfa_pred = post_bfa %>% posterior_predict(seed = 1, inv_trans_func = inv_logit)

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
