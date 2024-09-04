## Code to fit time-dependent beta latent variable models (time-BLV) 

- In the folder `analysis` there are several scripts used to fit the model.
- In the folder `packages` there are the packages developed to fit the model.

## Instructions 

### Packages

- To run the analysis, we need to install the packages some packages located in GitHub.
- Run `Rscript analysis/rscripts/install_analysis_packages.R` in the terminal to install them.

### config.yaml file 
- To generate the data and access Google Drive to upload and download the models, it is necessary to provide a file called `confit.yaml` at the root of the repository `tblv/`.
- The file has the following structure:

  ```{yaml}
  # Human Mortality database credentials
  hmd:
    username: 
    password: 
  
  # Google Drive credentials
  gdrive:
    auth_credentials: # auth credentials for the Google Drive API
    model_folder_id: # folder id for the model fit
    simulation_folder_id: # folder id for the simulation study
    test_model_folder_id: # folder id to run tests
    email: # Google drive account e-mail
  ```

### Fit and analyze the data

- To fit the time-BLV model to mortality data using a SLURM type cluster, run `Rscript analysis/rscripts/application/model_fit.R` on the terminal.
- The models will be stored on Google Drive (`model_folder_id`). Run `Rscript analysis/rscripts/application/download_models.R` on the terminal to download the models to your local machine. 
- Run `Rscript analysis/rscripts/application/fit_bfa_mortality_data.R` on the terminal to fit and save a Bayesian factor analysis applied to the log mortality.
- The following scripts were used to analyze the results:
  - [mortality_data_exploration.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/application/mortality_data_exploration.R) initial exploration of the dataset.   
  - [model_convergence_check.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/application/model_convergence_check.R) check HMC chains convergence.
  - [model_choice.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/application/model_choice.R) model choice for the latent dimension size.
  - [model_analysis.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/application/model_analysis.R) analyzes the posterior parameters of the time-BLV model for the selected dimension.
  - [model_comparison.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/application/model_comparison.R) compares the model to the baselines.

### Simulation Study
- Run `Rscript analysis/rscripts/simulation-study/simulate_data.R` on the terminal to generate the dataset based on the time-BLV and the true values estimated on the previous application.
- Run `Rscript analysis/rscripts/simulation-study/fit_sim_study.R` on the terminal to fit the simulation study on the server.
- The models will be stored on Google Drive (`simulation_folder_id`). Run `Rscript analysis/rscripts/simulation-study/download_models.R` on the terminal to download the models to your local machine.

- The following scripts were used to analyze the results:
  - [sim_study_convergence_check.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/simulation-study/sim_study_convergence_check.R) checks convergence of the HMC chains.   
  - [sim_study_model_choice.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/simulation-study/sim_study_model_choice.R) check models choice metrics on the simulated data.
  - [sim_study_analyzes.R](https://github.com/pedroaraujo9/tblv/blob/main/analysis/rscripts/simulation-study/sim_study_analysis.R) model choice for the latent dimension size.
 
