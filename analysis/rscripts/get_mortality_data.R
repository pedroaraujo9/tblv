library(HMDHFDplus)
library(tidyverse)
library(magrittr)
library(yaml)

# credentials with password and username to access HMD data
# https://www.mortality.org/
cred = yaml.load_file("analysis/rscripts/config.yaml")

# countries names 
countries = HMDHFDplus::getHMDcountries()

# read life tables for all countries and build a data.frame with relevant info
life_tables = map_df(countries$CNTRY, ~{
  readHMDweb(.x, 
             item = "bltper_5x5", 
             username = cred$username, 
             password = cred$password) %>%
    mutate(country_code = .x)
}) %>%
  select(Year, Age, mx, qx, OpenInterval, country_code) %>%
  left_join(countries %>% select(-link), by = c("country_code" = "CNTRY"))

names(life_tables) = names(life_tables) %>% tolower()

life_tables %>% glimpse()

### this data is available on the tblv package
saveRDS(life_tables, "analysis/data/life_tables_5x5.rds")
