library(btblv)
library(fitbtblv)


trueKs = c(2, 4)
repls = 1:30
K_max = 6

iter = 4000
warmup = 2000
thin = 5
chains = 3
mc_samples = 100000

config_path = "config.yaml"
gdrive_folder_id = "1rNyxQWOLd9MjxXkR2vWnin-l2Xvuu65j"
local_path = "analysis/models/simulation-study"

for(trueK in trueKs) {
  for(repl in repls) {
    
    model_name_pettern = paste0("trueK=", trueK, "-repl=", repl)
    data_path = paste0("analysis/data/simulation-study/sim_data_trueK=", 
                       trueK, "-replicate=", repl, ".rds")
    
    out = fit_save_btblv_models(
      K_max = K_max,
      cluster_run = TRUE,
      job_cores = job_cores,
      job_email = "pedro.menezesdearaujo@ucdconnect.ie",
      job_name = paste0("tK=", trueK, "r=", repl),
      btblv_data_path = data_path,
      iter = iter,
      warmup = warmup,
      thin = thin,
      chains = chains,
      precision = "single",
      seed = 1,
      mc_samples = mc_samples,
      config_path = config_path,
      model_name_pattern = model_name_pettern,
      save_gdrive = T,
      gdrive_folder_id = gdrive_folder_id,
      local_path = local_path
    )
    
  }
}

