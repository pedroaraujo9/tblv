library(btblv)

#### tblv single precision ####
path = "analysis/models"
models = list.files(path)
models = models[stringr::str_detect(models, "precision=single")]
models = paste0(path, "/", models)

conv_summary = lapply(1:10, function(k){
  print(k)
  model_path = models[stringr::str_detect(models, paste0("K=", k, ".rds"))]
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

conv_summary %>%
  select(param, rhat, K) %>%
  spread(param, rhat)

conv_summary %>%
  select(param, ess, K) %>%
  spread(param, ess)

