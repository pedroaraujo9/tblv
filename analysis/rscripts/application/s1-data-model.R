library(btblv)
library(tidyverse)

life_tables_5x1 = readRDS("analysis/data/life_tables_5x1.rds")
life_tables_5x5 = readRDS("analysis/data/life_tables_5x5.rds")

format_data = function(data, 
                       max_age, 
                       period_interval, 
                       period_gap, 
                       exclude_incomplete = TRUE) {
  
  data_model = data %>%
    filter(age <= max_age) %>%
    filter(year %in% seq(period_interval[1], period_interval[2], period_gap)) %>%
    filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                            "New Zealand Non-Maori", "England and Wales (Total Population)",
                            "England and Wales (Civilian Population)",
                            "Scotland", "Northern Ireland", "Wales",
                            "Luxembourg", "Iceland")))
  
  if(exclude_incomplete) {
    
    country_filter = data_model %>%
      group_by(country) %>%
      summarise(n = n()) %>%
      filter(n == max(n)) %>%
      .$country
    
    data_model = data_model %>% filter(country %in% country_filter)
    
  }
  
  
  return(data_model)
  
}

model_data_incomplete = life_tables_5x5 %>%
  format_data(max_age = 110, 
              period_interval = c(1950, 2015), 
              period_gap = 5, 
              exclude_incomplete = FALSE) 

model_data_complete = life_tables_5x5 %>%
  format_data(max_age = 110, 
              period_interval = c(1960, 2015), 
              period_gap = 5, 
              exclude_incomplete = TRUE) 

model_data_incomplete %>% saveRDS("analysis/data/model_data_incomplete.rds")
model_data_complete %>% saveRDS("analysis/data/model_data_complete.rds")

model_data_incomplete %>%
  filter(age < 110) %>%
  btblv::create_btblv_data("qx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_incomplete_data_qx.rds")

model_data_complete %>%
  filter(age < 110) %>%
  btblv::create_btblv_data("qx", "age", "country", "year") %>%
  saveRDS("analysis/data/btblv_complete_data_qx.rds")                    
