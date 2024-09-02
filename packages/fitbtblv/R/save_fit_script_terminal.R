args = commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  NULL
}else{

  args_list = list()

  for(i in 1:length(args)) {
    name_value = args[i] |>
      stringr::str_split("=", simplify = T, n = 2) |>
      as.character()

    args_list[name_value[1]] = name_value[2]
  }

  btblv_data_path = args_list$btblv_data_path
  K = args_list$K |> as.numeric()
  iter = args_list$iter |> as.numeric()
  warmup = args_list$warmup |> as.numeric()
  thin = args_list$thin |> as.numeric()
  chains = args_list$chains |> as.numeric()
  precision = args_list$precision
  seed = args_list$seed |> as.numeric()
  mc_samples = args_list$mc_samples |> as.numeric()
  config_path = args_list$config_path
  model_name_pattern = args_list$model_name_pattern
  save_gdrive = args_list$save_gdrive |> as.logical()
  gdrive_folder_id = args_list$gdrive_folder_id
  local_path = args_list$local_path

  fitbtblv::save_fit_btblv(
    btblv_data_path,
    K,
    iter,
    warmup,
    thin,
    chains,
    precision,
    seed,
    mc_samples,
    config_path,
    model_name_pattern,
    save_gdrive,
    gdrive_folder_id,
    local_path
  )
}
