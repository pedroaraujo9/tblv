library(tidyverse)
library(btblv)

sim_study_path = "analysis/models/simulation-study"
models = list.files(sim_study_path)
models = models[str_detect(models, "btblv-trueK")]
models = file.path(sim_study_path, models)
models


sim_conv_check = lapply(models, function(model) {
  print(model)
  fit =  model %>% readRDS()
  post = fit$btblv_fit %>% btblv::extract_posterior()
  conv_check = post %>% btblv::check_convergence()
  
  
  trueK = model %>% 
    stringr::str_extract("trueK=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  repl = model %>% 
    stringr::str_extract("repl=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  K = fit$btblv_fit$btblv_data$data_list_stan$K
  
  conv_check %>% purrr::imap(~{
    
    data.frame(
      prop_bad_rhat = mean(.x$rhat > 1.1),
      prop_bad_ess = mean(.x$ess < 10),
      param = .y,
      repl = repl, 
      trueK = trueK, 
      K = K
    )
    
  }) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)


sim_conv_check %>%
  filter(prop_bad_rhat > 0)
