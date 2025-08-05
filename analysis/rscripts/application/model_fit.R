library(fitbtblv)
library(btblv)

config_path = "config.yaml"
config = yaml::yaml.load_file(config_path)
googledrive::drive_auth_configure(path = "credentials.json")

job_cores = 30
K_max = 10
iter = 100
warmup = 50
thin = 1
chains = 3
gdrive_folder_id = config$gdrive$model_folder_id
mc_samples = 10000
local_path = "analysis/models/test"

mx_data_path = "analysis/data/btblv_data_mx_2010.rds"

#### mx fit ####
for(prec in c("single")) {

  out = fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = job_cores,
    job_email = config$gdrive$email,
    job_name = paste0("mx-2010", prec),
    btblv_data_path = mx_data_path,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = prec,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "config.yaml",
    model_name_pattern = "end-2010",
    save_gdrive = T,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path,
    service_account_path = "sakey.json"
  )

  print(out)

}


mx_data_path = "analysis/data/btblv_data_mx_2019.rds"

#### mx fit ####
for(prec in c("single")) {

  out = fit_save_btblv_models(
    K_max = K_max,
    cluster_run = TRUE,
    job_cores = job_cores,
    job_email = config$gdrive$email,
    job_name = paste0("mx-2019", prec),
    btblv_data_path = mx_data_path,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = prec,
    seed = 1,
    mc_samples = mc_samples,
    config_path = "config.yaml",
    model_name_pattern = "end-2019",
    save_gdrive = T,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path,
    service_account_path = "sakey.json"
  )

  print(out)

}

