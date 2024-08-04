## Code to fit time-dependent beta latent variable models (time-BLV)

- In the folder`analysis`, there are several scripts used to fit the model.
- In the folder `packages` there are the packages developed to fit the model.

## Instructions 

#### Packages

- To run the analysis, we need to install the packages in the `/packages` folder.
- Run `Rscript analysis/install_analysis_packages.R` in the terminal to install them.

#### Config file 
- To generate the data and access Google Drive to upload and download the models, it is necessary to provide a file called `confit.yaml` at the root of the repository `tblv/`.
- The file has the following structure:

  ```{yaml}
  # Human Mortality database credentials
  hmd:
    username: 
    password: 
  
  # Google Drive credentials
  gdrive:
    auth_credentials: 
    model_folder_id: 
    dev_model_folder_id: 
    email: 
  ```
