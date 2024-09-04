.compute_model_metrics = function(model_fit,
                                  mc_samples,
                                  cores,
                                  seed) {

  post = model_fit %>% btblv::extract_posterior(alpha_reference = "mode")

  appx = post %>%
    tblvArmaUtils::approx_mloglike(
      N = mc_samples,
      seed = seed,
      cores = cores
    )

  bic = tblvArmaUtils::compute_BIC(
    btblv_posterior = post,
    approx_mloglike = appx, cores = 1, seed = 1
  )

  waic = compute_WAIC(post)

  metrics = list(
    appx = appx,
    bic = bic,
    waic = waic
  )

  return(metrics)
}

.get_model_name = function(model_name_pattern, K, precision) {
  if(is.null(model_name_pattern)){

    model_name = paste0(
      "btblv-precision=", precision, "-",
      "K=", K, ".rds"
    )

  }else if(model_name_pattern == "") {

    model_name = paste0(
      "btblv-precision=", precision, "-",
      "K=", K, ".rds"
    )

  }else{

    model_name = paste0(
      "btblv-", model_name_pattern, "-precision=", precision, "-",
      "K=", K, ".rds"
    )
  }

  return(model_name)
}

.get_models_saved = function(save_gdrive,
                             local_path = NULL,
                             gdrive_folder_id = NULL) {

  if(save_gdrive == FALSE) {

    models_path = paste0(local_path, "/models-saved-list.txt")

    if(!file.exists(models_path)) {
      file.create(models_path)
    }

    models_saved = readLines(models_path)

    ##### Google drive #####
  }else{

    models_saved = googledrive::drive_ls(
      path = googledrive::as_id(gdrive_folder_id)
    )

    models_saved = models_saved$name

  }

  return(models_saved)
}


.get_bash_script = function(cluster_run,
                            job_cores = NULL,
                            job_email = NULL,
                            job_name = NULL,
                            K_max,
                            btblv_data_path,
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
                            local_path) {

  fit_save_r_script = .libPaths()[1] |>
    paste0("/fitbtblv/fit_save_btblv_terminal.R")

  r_script_args = glue::glue("
    btblv_data_path='{btblv_data_path}'
    iter={iter}
    warmup={warmup}
    thin={thin}
    chains={chains}
    precision='{precision}'
    seed={seed}
    mc_samples={mc_samples}
    config_path='{config_path}'
    model_name_pattern='{model_name_pattern}'
    save_gdrive={save_gdrive}
    gdrive_folder_id='{gdrive_folder_id}'
    local_path='{local_path}'"
  ) %>%
    as.character() %>%
    gsub("\n", " ", .)

  if(cluster_run == FALSE) {

    bash_script = glue::glue("
      #!/bin/bash -l

      for K in $(seq 1 {K_max})
        do
          Rscript {fit_save_r_script} K=$K --args {r_script_args};
        done
      "
    )

  }else{
    bash_script = glue::glue(
      "
      #!/bin/bash -l

      # Set the number of nodes

      #SBATCH -N 1

      # Set the number of tasks/cores per node required
      #SBATCH -n {job_cores}

      # Set the walltime of the job to 1 hour (format is hh:mm:ss)
      #SBATCH -t 300:00:00

      # E-mail on begin (b), abort (a) and end (e) of job
      #SBATCH --mail-type=ALL

      # E-mail address of recipient
      #SBATCH --mail-user={job_email}

      # Specifies the jobname
      #SBATCH --job-name={job_name}

      for K in $(seq 1 {K_max})
      do
        Rscript {fit_save_r_script} K=$K --args {r_script_args} &
      done

      wait;
      "
    )

  }

  return(bash_script)
}
