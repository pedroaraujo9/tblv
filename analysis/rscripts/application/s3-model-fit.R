library(fitbtblv)
library(btblv)

config_path = "config.yaml"
config = yaml::yaml.load_file(config_path)

job_cores = 30
K_max = 10
iter = 10000
warmup = 5000
thin = 10
chains = 3
gdrive_folder_id = config$gdrive$model_folder_id
mc_samples = 100000
local_path = "analysis/models/test"

qx_data_path = "analysis/data/btblv_data_qx.rds"
mx_data_path = "analysis/data/btblv_data_mx.rds"

#### mx fit ####
for(prec in c("single", "specific")) {

  out = fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = job_cores,
    job_email = config$gdrive$email,
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

  print(out)

}

#### qx fit ####
for(prec in c("specific", "single")) {

  out = fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = job_cores,
    job_email = config$gdrive$email,
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

  print(out)

}

