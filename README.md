## Code to fit time-dependent beta latent variable models (time-BLV)

- In the folder`analysis` there are several scripts used to fit the model.
- In the folder `packages` there are the packages developed to fit the model.

## Instructions 

#### Packages

- To run the analysis, we need to install the packages in the `/packages` folder.
- Run `Rscript analysis/rscripts/install_analysis_packages.R` in the terminal to install them.

#### config.yaml file 
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
    dev_model_folder_id: # folder id to run tests
    simulation_folder_id: # folder id for the simulation study
    email: # Google drive account e-mail
  ```
