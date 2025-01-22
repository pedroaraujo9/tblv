library(googledrive)
library(tidyverse)
library(yaml)
source("analysis/rscripts/utils.R")

config = yaml::yaml.load_file("config.yaml")

# download models 
download_models_gdrive(
  gdrive_auth_credentials = config$gdrive$auth_credentials, 
  gdrive_auth_email = config$gdrive$email, 
  gdrive_folder_id = "1mRwJozxCbobGbZQcLCkh8rXRNBICl9w0", 
  local_folder_path = "analysis/models"
)


m1 = readRDS("analysis/models/btblv-mx-1x1-precision=specific-K=4.rds")
m2 = readRDS("analysis/models/btblv-mx-1x1-precision=single-K=4.rds")

m1$metrics$bic
m2$metrics$bic

pm1 = m1$btblv_fit %>% extract_posterior()
pm2 = m2$btblv_fit %>% extract_posterior()

c1 = pm1 %>% btblv::check_convergence()
c2 = pm2 %>% btblv::check_convergence()

pm1$post_sample_chains$beta[, , 1] %>% 
  as.data.frame() %>%
  mutate(iter = 1:nrow(.)) %>%
  gather(chain, value, -iter) %>%
  ggplot(aes(x=iter, y=value, color=chain)) + 
  geom_line()

c1$beta
c2$beta

mm = lapply(1:10, FUN = function(k){
  fit = readRDS(paste0("analysis/models/btblv-mx-1x1-precision=single-K=", k, ".rds")) 
  post = fit$btblv_fit %>% extract_posterior()
  conv = post %>% check_convergence()
  
  list(metrics = fit$metrics, conv = conv)
})

lapply(mm, function(x){
  x$metrics$bic
}) |>
  do.call(c, args = _) %>%
  plot()







