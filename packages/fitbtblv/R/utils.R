#' Compute Model Metrics
#'
#' This function computes various model metrics, including the approximate marginal log-likelihood,
#' Bayesian Information Criterion (BIC), and Widely Applicable Information Criterion (WAIC),
#' for a fitted model.
#'
#' @param model_fit A fitted model object from which posterior samples can be extracted.
#' @param mc_samples Integer. The number of Monte Carlo samples to use for approximating the
#'   marginal log-likelihood.
#' @param cores Integer. The number of CPU cores to use for parallel computation.
#' @param seed Integer. A seed for reproducibility of the Monte Carlo sampling.
#'
#' @return A list containing the following metrics:
#' \itemize{
#'   \item `appx`: The approximate marginal log-likelihood.
#'   \item `bic`: The Bayesian Information Criterion (BIC).
#'   \item `waic`: The Widely Applicable Information Criterion (WAIC).
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' model_fit = ... # A fitted model object
#' metrics = .compute_model_metrics(
#'   model_fit = model_fit,
#'   mc_samples = 1000,
#'   cores = 4,
#'   seed = 123
#' )
#' print(metrics)
#' }
#'
#' @importFrom btblv extract_posterior compute_WAIC
#' @importFrom tblvArmaUtils approx_mloglike compute_BIC
#' @importFrom magrittr %>%
.compute_model_metrics = function(model_fit,
                                  mc_samples,
                                  cores,
                                  seed) {

  post = model_fit |> btblv::extract_posterior(alpha_reference = "mode")

  appx = post |>
    tblvArmaUtils::approx_mloglike(
      N = mc_samples,
      seed = seed,
      cores = cores
    )

  bic = tblvArmaUtils::compute_BIC(
    btblv_posterior = post,
    approx_mloglike = appx, cores = 1, seed = 1
  )

  waic = btblv::compute_WAIC(post)

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

#' Get Saved Models List
#'
#' This function retrieves a list of saved models either from a local file or from a Google Drive folder.
#' If the models are stored locally, it reads from a specified text file. If stored on Google Drive,
#' it lists the files in a specified folder.
#'
#' @param save_gdrive Logical. If `TRUE`, the function retrieves the list of models from Google Drive.
#'   If `FALSE`, it retrieves the list from a local file.
#' @param local_path Character. The local directory path where the models list is stored. Required if
#'   `save_gdrive = FALSE`.
#' @param gdrive_folder_id Character. The Google Drive folder ID where the models are stored. Required if
#'   `save_gdrive = TRUE`.
#'
#' @return A character vector containing the names of the saved models.
#'
#' @examples
#' \dontrun{
#' # Example 1: Retrieve models from a local file
#' local_path = "/path/to/local/folder"
#' models_saved = .get_models_saved(
#'   save_gdrive = FALSE,
#'   local_path = local_path
#' )
#' print(models_saved)
#'
#' # Example 2: Retrieve models from Google Drive
#' gdrive_folder_id = "your-google-drive-folder-id"
#' models_saved = .get_models_saved(
#'   save_gdrive = TRUE,
#'   gdrive_folder_id = gdrive_folder_id
#' )
#' print(models_saved)
#' }
#'
#' @importFrom googledrive drive_ls as_id
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

#' Generate Bash Script for Model Fitting
#'
#' This function generates a bash script to fit and save Bayesian models either locally or on a cluster.
#' The script can be customized for local execution or for submission to a job scheduler (e.g., SLURM).
#'
#' @param cluster_run Logical. If `TRUE`, the bash script is configured for cluster execution using a job
#'   scheduler (e.g., SLURM). If `FALSE`, the script is configured for local execution.
#' @param job_cores Integer. The number of CPU cores to request for the job (required if `cluster_run = TRUE`).
#' @param job_email Character. The email address to receive job notifications (required if `cluster_run = TRUE`).
#' @param job_name Character. The name of the job (required if `cluster_run = TRUE`).
#' @param K Integer vector. The numbers of clusters to fit models for. Can be a
#'   single value or a vector of values.
#' @param btblv_data_path Character. The path to the data file for model fitting.
#' @param iter Integer. The number of iterations for the MCMC sampling.
#' @param warmup Integer. The number of warmup iterations for the MCMC sampling.
#' @param thin Integer. The thinning interval for the MCMC sampling.
#' @param chains Integer. The number of chains for the MCMC sampling.
#' @param precision Character. The precision parametrization for the model. It can be `"specific"` or `"single"`.
#' @param seed Integer. The seed for reproducibility.
#' @param mc_samples Integer. The number of Monte Carlo samples for model diagnostics.
#' @param config_path Character. The path to the configuration file.
#' @param model_name_pattern Character. The pattern for naming the saved model files.
#' @param save_gdrive Logical. If `TRUE`, the models are saved to Google Drive. If `FALSE`, they are saved locally.
#' @param gdrive_folder_id Character. The Google Drive folder ID for saving models (required if `save_gdrive = TRUE`).
#' @param local_path Character. The local path for saving models (required if `save_gdrive = FALSE`).
#' @param max_treedepth Integer. The maximum tree depth for the MCMC sampling (default: 10).
#'
#' @return A character string containing the generated bash script.
#'
#' @examples
#' \dontrun{
#' # Example 1: Generate a bash script for local execution
#' bash_script = .get_bash_script(
#'   cluster_run = FALSE,
#'   K = 1:5,
#'   btblv_data_path = "/path/to/data.csv",
#'   iter = 2000,
#'   warmup = 1000,
#'   thin = 1,
#'   chains = 4,
#'   precision = "double",
#'   seed = 123,
#'   mc_samples = 1000,
#'   config_path = "/path/to/config.yaml",
#'   model_name_pattern = "model_K",
#'   save_gdrive = FALSE,
#'   local_path = "/path/to/save/models"
#' )
#' cat(bash_script)
#'
#' # Example 2: Generate a bash script for cluster execution
#' bash_script = .get_bash_script(
#'   cluster_run = TRUE,
#'   job_cores = 4,
#'   job_email = "user@example.com",
#'   job_name = "btblv_fit",
#'   K = 1:5,
#'   btblv_data_path = "/path/to/data.csv",
#'   iter = 2000,
#'   warmup = 1000,
#'   thin = 1,
#'   chains = 4,
#'   precision = "double",
#'   seed = 123,
#'   mc_samples = 1000,
#'   config_path = "/path/to/config.yaml",
#'   model_name_pattern = "model_K",
#'   save_gdrive = TRUE,
#'   gdrive_folder_id = "your-google-drive-folder-id"
#' )
#' cat(bash_script)
#' }
#'
#' @importFrom glue glue
.get_bash_script = function(cluster_run,
                            job_cores = NULL,
                            job_email = NULL,
                            job_name = NULL,
                            K,
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
                            local_path,
                            max_treedepth = 10) {

  fit_save_r_script = .libPaths()[1] |>
    paste0("/fitbtblv/fit_save_btblv_terminal.R")

  K_values = paste(K, collapse = " ")

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
    local_path='{local_path}'
    max_treedepth='{max_treedepth}'"
  ) |>
    as.character() |>
    gsub("\n", " ", x = _)

  if(cluster_run == FALSE) {

    bash_script = glue::glue("
      #!/bin/bash -l

      for K in {K_values}
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

      for K in {K_values}
      do
        Rscript {fit_save_r_script} K=$K --args {r_script_args} &
      done

      wait;
      "
    )

  }

  return(bash_script)
}
