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
    post_sample = model_fit %>% 
      extract_posterior(alpha_reference = "posterior mode")
    
    model_waic = post_sample %>% compute_WAIC() %>% .$waic
    model_bic = post_sample %>% compute_BIC(N = 10000, seed = 1, cores = 5)
    
    data.frame(
      K = k,
      WAIC = model_waic,
      BIC = model_bic
    )
  }) %>%
    do.call(rbind, .)
  
  return(metrics_summ)
}

#### compute metrics ####
path = "analysis/models"
models = list.files(path)
models_path = paste0(path, "/", models)
models_path = models_path[stringr::str_detect( models_path, "btblv-")]
models_path

metrics_path = paste0("analysis/results/", list.files("analysis/results"))
metrics_path

# single precision
single_prec_path = "analysis/results/model_choice_metrics_single_prec.rds"

if(!(single_prec_path %in% metrics_path)) {
  metrics_single_prec = compute_metrics(models_path, "single")
}else{
  metrics_single_prec = readRDS(single_prec_path)
}

saveRDS(metrics_single_prec, single_prec_path)

# specific precision
specific_prec_path = "analysis/results/model_choice_metrics_specific_prec.rds"

if(!(specific_prec_path %in% metrics_path)) {
  metrics_specific_prec = compute_metrics(models_path, "specific")
}else{
  metrics_specific_prec = readRDS(specific_prec_path)
}

saveRDS(metrics_single_prec, specific_prec_path)

# analysis
metrics_single_prec
metrics_single_prec$BIC %>% plot()
metrics_single_prec$WAIC %>% plot()

metrics_specific_prec
metrics_specific_prec$BIC %>% plot()
metrics_specific_prec$WAIC %>% plot()

