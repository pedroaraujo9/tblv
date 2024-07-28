library(HMDHFDplus)
library(tidyverse)
library(magrittr)
library(yaml)

# credentials with password and username to access HMD data
# https://www.mortality.org/
cred = yaml.load_file("analysis/rscripts/config.yaml")

# countries names 

# read life tables for all countries and build a data.frame with relevant info
download_mortality = function(credentials, age_group_size, cohort_size) {
  countries = HMDHFDplus::getHMDcountries()
  life_tables = map_df(countries$CNTRY, ~{
    readHMDweb(.x, 
               item = paste0("bltper_", age_group_size, "x", cohort_size), 
               username = credentials$username, 
               password = credentials$password) %>%
      mutate(country_code = .x)
  }) %>%
    select(Year, Age, mx, qx, OpenInterval, country_code) %>%
    left_join(countries %>% select(-link), by = c("country_code" = "CNTRY"))
  
  names(life_tables) = names(life_tables) %>% tolower()
  life_tables$date_extract = Sys.Date()
  return(life_tables)
}

## life tables period data
download_mortality(cred, 1, 1) %>% saveRDS("analysis/data/life_tables_1x1.rds")
download_mortality(cred, 1, 5) %>% saveRDS("analysis/data/life_tables_1x5.rds")
download_mortality(cred, 1, 10) %>% saveRDS("analysis/data/life_tables_1x10.rds")

download_mortality(cred, 5, 1) %>% saveRDS("analysis/data/life_tables_5x1.rds")
download_mortality(cred, 5, 5) %>% saveRDS("analysis/data/life_tables_5x5.rds")
download_mortality(cred, 5, 10) %>% saveRDS("analysis/data/life_tables_5x10.rds")

