library(btblv)
library(tidyverse)


data = readRDS("analysis/data/data_model.rds")

train = data %>%
  filter(year <= 2000)

test = data %>%
  filter(year > 2000)


btblv_data = btblv::create_btblv_data(
  train, "mx", "age", "country", "year"
)

btblv_data

fit = fit_btblv(
  btblv_data = btblv_data, 
  K = 4, 
  precision = "single", 
  iter = 1000, 
  warmup = 500, 
  thin = 5, 
  chains = 3, 
  cores = 3, 
  seed = 1 
)

post = fit %>% btblv::extract_posterior()
conv = post %>% check_convergence()
conv$beta
conv$phi

countries_forecast_times = test %>%
  select(country, year) %>%
  distinct() 

countries = countries_forecast_times$country %>% unique()

K = fit$btblv_data$data_list_stan$K
J = fit$btblv_data$data_list_stan$J

btblv_data$data %>%
  select(group, group_num, time, time_num, ind_num) %>%
  distinct() %>%
  filter(group == "Croatia") %>%
  filter(time == 2000)

alpha = post$post_sample_array$rot_alpha
beta = post$post_sample_array$beta
kappa = post$post_sample_array$kappa

last_time

preds = lapply(countries, function(country_sel){
  print(country_sel)
  
  last_time = btblv_data$data %>%
    select(group, group_num, time, time_num, ind_num) %>%
    distinct() %>%
    filter(group == country_sel) %>%
    filter(time == 2000) 
  
  if(nrow(last_time) != 0) {
    country_forecast_times = countries_forecast_times %>% filter(country == country_sel)
    forecast_times = country_forecast_times$year %>% sort()
    
    theta = post$post_sample_array$rot_theta[, last_time$ind_num, ]
    phi = post$post_sample_array$phi[, last_time$group_num]
    sigma = post$post_sample_array$sigma[, last_time$group_num]
    
    iters = nrow(theta)
    pred_list = list()
    new_theta = theta
    
    
    for(itime in seq_along(forecast_times)) {
      
      for(k in 1:K) {
        new_theta[, k] = phi*theta[, k] + sigma*rnorm(n=length(phi))
      }
      
      pred = matrix(nrow = iters, ncol = J)
      
      for(j in 1:J) {
        pred[, j] = rowSums(theta*alpha[, j, ]) 
      }
      
      pred_list[[itime]] = data.frame(pred) %>%
        mutate(iter = 1:nrow(.)) %>%
        gather(item_num, pred, -iter) %>%
        mutate(item_num = item_num %>% str_extract("\\d{1,10}") %>% as.numeric()) %>%
        mutate(year = forecast_times[itime], country = country_sel) %>%
        as_tibble()
      
      theta = new_theta
    }
    
    pred_list %>%
      do.call(rbind, .) %>%
      left_join(
        btblv_data$data %>% select(item_num, item) %>% distinct(),
        by = "item_num",
      ) %>%
      rename(age = item)
  }else{
    return(NULL)
  }
  
}) %>% do.call(rbind, .)

dev_pred = preds %>%
  group_by(year, country, age) %>%
  summarise(pred_mean = mean(pred)) %>%
  filter(age == 0, country == "Australia")
  
dev = numeric(11)
for(t in 1:11) {
  dev[t] = rowSums(post$post_sample_array$rot_theta[, t, ]*post$post_sample_array$rot_alpha[, 1, ]) %>% mean()
}

c(dev, dev_pred$pred_mean) %>% plot()


preds_true = preds %>%
  group_by(year, country, age) %>%
  summarise(pred_mean = mean(pred)) %>%
  left_join(test, by=c("year", "country", "age"))

preds_true %>%
  filter(country == "Australia", age == 10) 

last_time

country_forecast_times = countries_forecast_times %>% filter(country == country_sel)
forecast_times = country_forecast_times$year %>% sort()



theta = post$post_sample_array$rot_theta[, last_time$ind_num, ]
phi = post$post_sample_array$phi[, last_time$group_num]
sigma = post$post_sample_array$sigma[, last_time$group_num]
alpha = post$post_sample_array$rot_alpha
beta = post$post_sample_array$beta
kappa = post$post_sample_array$kappa

K = fit$btblv_data$data_list_stan$K
J = fit$btblv_data$data_list_stan$J
iters = nrow(theta)
pred_list = list()


for(itime in seq_along(forecast_times)) {
  new_theta = theta
  
  for(k in 1:K) {
    new_theta[, k] = phi*theta[, k] + sigma*rnorm(n=length(phi))
  }
  
  pred = matrix(nrow = iters, ncol = J)
  
  for(j in 1:J) {
    pred[, j] = inv_logit(beta[, j] + rowSums(theta*alpha[, j, ])) 
  }
  
  pred_list[[itime]] = data.frame(pred) %>%
    mutate(iter = 1:nrow(.)) %>%
    gather(item, pred, -iter) %>%
    mutate(item = item %>% str_extract("\\d{1,10}") %>% as.numeric()) %>%
    mutate(year = forecast_times[itime], country = country_sel) %>%
    as_tibble()
  
  theta = new_theta
}

pred_list %>%
  do.call(rbind, .)


