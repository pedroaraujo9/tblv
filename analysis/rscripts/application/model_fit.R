devtools::install_github(repo = "pedroaraujo9/tblv", subdir = "fitbtblv")

library(fitbtblv)
library(btblv)

K_max = 10
iter = 30
warmup = 15
thin = 1
chains = 2
config_path = "config.yaml"
gdrive_folder_id = "1LvmQrUG3P424ZsEVi_LuWfgUHoLmf-LH"
mc_samples = 1000
local_path = "analysis/models/test"

qx_data_path = "analysis/data/btblv_data_qx.rds"
mx_data_path = "analysis/data/btblv_data_mx.rds"

#### mx fit ####
for(prec in c("single", "specific")) {
  fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = 20,
    job_email = "pedro.menezesdearaujo@ucdconnect.ie",
    job_name = paste0("mx-", prec),
    btblv_data_path = mx_data_path,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = prec,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "config.yaml",
    model_name_pattern = "",
    save_gdrive = T,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path
  )
}

#### qx fit ####
for(prec in c("single", "specific")) {
  fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = 20,
    job_email = "pedro.menezesdearaujo@ucdconnect.ie",
    job_name = paste0("qx-", prec),
    btblv_data_path = qx_data_path,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = prec,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "config.yaml",
    model_name_pattern = "qx",
    save_gdrive = T,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path
  )
}

