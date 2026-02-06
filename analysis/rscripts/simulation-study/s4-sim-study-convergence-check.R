library(tidyverse)
library(btblv)
library(foreach)
library(doParallel)

get_convergence_info = function(model_path) {
  
  print(model_path)
  
  fit =  model_path %>% readRDS()
  post = fit$btblv_fit %>% btblv::extract_posterior()
  conv_check = post %>% btblv::check_convergence()
  
  
  trueK = model_path %>% 
    stringr::str_extract("trueK=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  repl = model_path %>% 
    stringr::str_extract("repl=\\d{1,10}") %>% 
    stringr::str_extract("\\d{1,10}") %>% 
    as.numeric() 
  
  K = fit$btblv_fit$btblv_data$data_list_stan$K
  
  conv_check %>% purrr::imap(~{
    
    data.frame(
      prop_rhat = mean(.x$rhat > 1.1),
      rhat_q90 = quantile(.x$rhat, 0.9),
      rhat_q95 = quantile(.x$rhat, 0.95),
      rhat_q99 = quantile(.x$rhat, 0.99),
      rhat_max = max(.x$rhat),
      prop_ess = mean(.x$ess < 30),
      ess_q10 = quantile(.x$ess, 0.1),
      ess_q5 = quantile(.x$ess, 0.05),
      ess_q1 = quantile(.x$ess, 0.01),
      ess_min = min(.x$ess),
      param = .y,
      repl = repl, 
      trueK = trueK, 
      K = K
    )
    
  }) %>% do.call(rbind, .)
}

sim_study_path = "analysis/models/simulation-qx"
models = list.files(sim_study_path)
models = models[str_detect(models, "btblv-trueK")]
models = file.path(sim_study_path, models)
print(models)

sim_conv_check = lapply(models, function(model_path){
  get_convergence_info(model_path)
})

saveRDS(sim_conv_check, file = "analysis/results/sim-conv-check.rds")

sim_conv_check = readRDS("analysis/results/sim-conv-check.rds")

# Corrected Greek label list 
greek_labs = c(
  "alpha" = "alpha", 
  "beta" = "beta", 
  "theta" = "theta",
  "log_kappa" = "log~kappa", 
  "phi" = "phi", 
  "sigma" = "sigma"
)

# Define the NEW custom labeller function
custom_labeller_fixed <- function(labels) {
  
  # 1. Identify which column is 'param' and which is 'trueK'
  
  # Handle 'param' labels (for Greek letters)
  if ("param" %in% names(labels)) {
    # Replace parameter names with their Greek expression strings
    param_labels <- data.frame(param = greek_labs[labels$param])
    # Apply label_parsed to render them as expressions
    param_labels <- label_parsed(param_labels)
  } else {
    param_labels <- NULL
  }
  
  # Handle 'trueK' labels (for "True K = X")
  if ("trueK" %in% names(labels)) {
    # Use label_value for standard text labels
    trueK_labels <- label_value(data.frame(trueK = labels$trueK))
  } else {
    trueK_labels <- NULL
  }
  
  # 2. Combine and return the label list
  # Coalesce/Combine the resulting lists of labels (or use map_dfc if using purrr)
  if (!is.null(param_labels) && !is.null(trueK_labels)) {
    # This handles the case where facets are crossed (trueK ~ param)
    return(c(trueK_labels, param_labels))
  } else if (!is.null(param_labels)) {
    # This handles a single facet dimension 'param'
    return(param_labels)
  } else if (!is.null(trueK_labels)) {
    # This handles a single facet dimension 'trueK'
    return(trueK_labels)
  }
  
  # Fallback to default if something unexpected happens
  return(label_value(labels))
}

sim_conv_check %>% 
  do.call(rbind, .) %>%
  select(param, repl, trueK, K, rhat_q99, rhat_max, prop_rhat) %>%
  as_tibble() %>%
  filter(param %in% c("alpha", "beta", "log_kappa", "sigma", "phi","theta")) %>%
  # trueK preparation
  mutate(trueK = paste0("True K = ", trueK)) %>% 
  ggplot(aes(x = factor(K), y = rhat_q99)) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  # Apply the new custom_labeller_fixed function
  facet_wrap(trueK ~ param, 
             scales = "free", 
             labeller = custom_labeller_fixed, 
             ncol = 3) + 
  labs(x = "Fitted K", y = "99th percentile of R-hat")

ggsave("analysis/plots/sim-study-rhat-q99.pdf", width = 8, height = 8)

sim_conv_check %>% 
  do.call(rbind, .) %>%
  select(param, repl, trueK, K, ess_q1, rhat_max, prop_rhat) %>%
  as_tibble() %>%
  filter(param %in% c("alpha", "beta", "log_kappa", "sigma", "phi","theta")) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x = factor(K), y = log(ess_q1))) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  facet_wrap(trueK ~ param, 
             scales = "free", 
             labeller = custom_labeller_fixed, 
             ncol = 3) + 
  labs(x = "Fitted K", y = "1st percentile of ESS (log scale)")

ggsave("analysis/plots/sim-study-ess-q1.pdf", width = 8, height = 8)





