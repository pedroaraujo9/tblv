#' Run btblv model for different K locally or in a cluster.
#'
#' @param K_max integer with max K to be fitted.
#' @param cluster_run logical, if `TRUE` fits the model in parallel in the terminal.
#' @param job_cores integer with the number of cores.
#' @param job_email character with the email for cluster messages.
#' @param job_name character with the job name.
#' @param btblv_data_path character with the path to the `btblv::btblv_data` object.
#' @param iter integer with the number of HMC iterations.
#' @param warmup integer with the number of warmup iterations.
#' @param thin integer with the number of thinning.
#' @param chains integer with the number of chains.
#' @param precision character with the type of precision. It can be "single" or "specific".
#' @param seed integer with the random seed.
#' @param mc_samples integer with the number of Monte Carlo samples for the BIC.
#' @param config_path character with the config.yaml file path.
#' @param model_name_pattern character with the pattern to save the model.
#' @param save_gdrive logical indicating if the model should be saved on Google drive.
#' @param gdrive_folder_id character with the Google Drive folder id.
#' @param local_path character with local folder to save temporary and permanent files.
#'
#' @return `TRUE` if the terminal code ran, `FALSE` otherwise.
#' @export
#'
#' @examples
#'  ##
run_fit = function(K_max,
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
                   local_path) {

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
    local_path = local_path
  )

  if(cluster_run == FALSE) {

    system(bash_script)

  }else{

    bash_file_path = paste0(local_path, "/run_fit_server_bash_script.sh")
    cat(bash_script, file = bash_file_path)

    system(paste0("sbatch ", bash_file_path))

  }

  return(TRUE)
}
