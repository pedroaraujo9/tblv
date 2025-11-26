library(btblv)
library(tidyverse)

convergence_summary = function(models_path, precision_type) {
  
  precision_match = paste0("precision=", precision_type)
  models_path = models_path[stringr::str_detect(models_path,  precision_match)]
  
  Ks = models_path %>%
    stringr::str_extract("K=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>%
    as.integer() %>%
    na.omit() %>%
    sort()
  
  conv_summary = lapply(Ks, function(k){
    model_path = models_path[stringr::str_detect(models_path, paste0("K=", k, ".rds"))]
    print(model_path)
    
    model_fit = readRDS(model_path)
    
    # convergence statistics 
    conv_stats = model_fit %>% 
      btblv::extract_posterior() %>%
      btblv::check_convergence()
    
    # bad ones for each class of parameters
    prop_bad = lapply(names(conv_stats), function(param){
      data.frame(
        rhat = mean(conv_stats[[param]]$rhat > 1.1),
        ess = mean(conv_stats[[param]]$ess < 10),
        param = param,
        K = k
      )
    }) %>%
      do.call(rbind, .) %>%
      tibble::as_tibble()
    
    return(prop_bad)
    
  }) %>%
    do.call(rbind, .)
  
  rhat_summary = conv_summary %>%
    dplyr::select(param, rhat, K) %>%
    tidyr::spread(param, rhat)

  ess_summary = conv_summary %>%
    dplyr::select(param, ess, K) %>%
    tidyr::spread(param, ess)
  
  out = list(
    rhat_summary = rhat_summary,
    ess_summary = ess_summary
  )
  
  return(out)
}


#### check convergence stats ####
path = "analysis/models"
models = list.files(path)
models_path = paste0(path, "/", models)
models_path = models_path[stringr::str_detect( models_path, "btblv-")]
models_path

# single precision
conv_summary = convergence_summary(models_path, precision_type = "single")
conv_summary %>% saveRDS("analysis/models/convergence_summary.rds")
conv_summary = readRDS("analysis/models/convergence_summary.rds")

conv_summary$rhat_summary
conv_summary$ess_summary

conv_summary$rhat_summary %>%
  as.data.frame() %>%
  select(alpha, beta, log_kappa, phi, sigma, theta) %>%
  round(2) %>%
  xtable::xtable()

conv_summary$ess_summary %>%
  as.data.frame() %>%
  select(alpha, beta, log_kappa, phi, sigma, theta) %>%
  round(2) %>%
  xtable::xtable()

# specific precision
conv_summary = convergence_summary(models_path, precision_type = "specific")

conv_summary$rhat_summary
conv_summary$ess_summary

