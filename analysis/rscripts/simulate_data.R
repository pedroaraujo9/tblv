library(btblv)

for(K in c(2, 4)) {
  
  fit = readRDS(
    paste0("analysis/models/btblv-precision=single-K=", K, ".rds")
  )
  
  post = fit %>% btblv::extract_posterior()
  sim_data = post %>% btblv::simulate_data(replicates = 30, seed = 1)
  
  for(i in 1:length(sim_data$sim_data_list)) {
    file_name = paste0(
      "analysis/data/simulation-study/sim_data_trueK=", K,
      "-replicate=", i, ".rds"
    )
    
    sim_data$sim_data_list[[i]] %>%
      saveRDS(file_name)
  }  
}
