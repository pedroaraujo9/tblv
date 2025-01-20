#' Fit and Save Bayesian Models for BTBLV
#'
#' This function prepares and executes a bash script to fit Bayesian models
#' for BTBLV data either locally or on a computing cluster. The script is
#' generated dynamically based on user-specified parameters.
#'
#' @param K_max Integer. The maximum number of clusters to consider.
#' @param cluster_run Logical. If `TRUE`, the script is executed on a cluster using `sbatch`.
#' @param job_cores Integer. Number of cores to allocate for the job. Only applicable for cluster runs.
#' @param job_email Character. Email address for job notifications. Only applicable for cluster runs.
#' @param job_name Character. Name of the job. Only applicable for cluster runs.
#' @param btblv_data_path Character. Path to the BTBLV data file.
#' @param iter Integer. Number of iterations for model fitting.
#' @param warmup Integer. Number of warmup iterations for model fitting.
#' @param thin Integer. Thinning interval for model fitting.
#' @param chains Integer. Number of chains for model fitting.
#' @param precision Character. Precision model type, it can be `single` or `specific`.
#' @param seed Integer. Seed for random number generation.
#' @param mc_samples Integer. Number of Monte Carlo samples to generate.
#' @param config_path Character. Path to the configuration file.
#' @param model_name_pattern Character. Pattern for naming the models.
#' @param save_gdrive Logical. If `TRUE`, results are saved to Google Drive.
#' @param gdrive_folder_id Character. Folder ID for saving results in Google Drive. Required if `save_gdrive` is `TRUE`.
#' @param local_path Character. Local directory path to save results and scripts.
#' @param max_treedepth Integer. Maximum tree depth for Stan's Hamiltonian Monte Carlo sampler. Default is 10.
#'
#' @return Character vector. The output from running the bash script.
#'
#' @details
#' - When `cluster_run` is `FALSE`, the script is executed locally using the `system` command.
#' - When `cluster_run` is `TRUE`, the script is saved as a bash file and submitted to a cluster via `sbatch`.
#' - Ensure the folder specified in `local_path` exists before running the function.
#'
#' @examples
#' \dontrun{
#'   fit_save_btblv_models(
#'     K_max = 5,
#'     cluster_run = TRUE,
#'     job_cores = 4,
#'     job_email = "user@example.com",
#'     job_name = "btblv_model",
#'     btblv_data_path = "data/btblv_data.csv",
#'     iter = 2000,
#'     warmup = 1000,
#'     thin = 1,
#'     chains = 4,
#'     precision = 0.01,
#'     seed = 1234,
#'     mc_samples = 100,
#'     config_path = "config.yaml",
#'     model_name_pattern = "model_K",
#'     save_gdrive = TRUE,
#'     gdrive_folder_id = "1A2B3C4D5E6F7G8H9I",
#'     local_path = "output",
#'     max_treedepth = 12
#'   )
#' }
#'
#' @importFrom assertthat assert_that
#' @export

fit_save_btblv_models = function(K_max,
                                 cluster_run,
                                 job_cores = NULL,
                                 job_email = NULL,
                                 job_name = NULL,
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

  assertthat::assert_that(
    file.exists(local_path), msg = "folder in `local_path` does not exists."
  )

  bash_script = .get_bash_script(
    cluster_run = cluster_run,
    job_cores = job_cores,
    job_email = job_email,
    job_name = job_name,
    K_max = K_max,
    btblv_data_path = btblv_data_path,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    precision = precision,
    seed = seed,
    mc_samples = mc_samples,
    config_path = config_path,
    model_name_pattern = model_name_pattern,
    save_gdrive = save_gdrive,
    gdrive_folder_id = gdrive_folder_id,
    local_path = local_path,
    max_treedepth = max_treedepth
  )

  if(cluster_run == FALSE) {

    out = system(bash_script, intern = TRUE)

  }else{

    bash_file_path = paste0(local_path, "/run_fit_server_bash_script.sh")
    cat(bash_script, file = bash_file_path)

    out = system(paste0("sbatch ", bash_file_path), intern = TRUE)

  }

  return(out)
}
