library(btblv)

for(K in c(2, 4)) {
  
  fit = readRDS(
    paste0("analysis/models/btblv-qx-incomplete-precision=single-K=", K, ".rds")
  )
  
  post = fit$btblv_fit |> btblv::extract_posterior()
  sim_data = post |> btblv::simulate_data(replicates = 50, seed = 1)
  
  saveRDS(sim_data, paste0("analysis/data/sim_data_", K, ".rds"))
  
  for(i in 1:length(sim_data$sim_data_list)) {
    file_name = paste0(
      "analysis/data/simulation-study/sim_data_trueK=", K,
      "-replicate=", i, ".rds"
    )
    
    sim_data$sim_data_list[[i]] |> 
      btblv::create_btblv_data("qx", "age", "country", "year") |>
      saveRDS(file_name)
  }  
}
