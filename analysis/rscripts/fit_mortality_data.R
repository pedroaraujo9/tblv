library(btblv)

#### getting terminal parameters ####
args = commandArgs(trailingOnly = T)[-1]
args_list = list()

for(i in 1:length(args)) {
  name_value = args[i] %>% str_split("=", simplify = T) %>% as.character()
  args_list[name_value[1]] = name_value[2] %>% as.numeric()
}

print(args_list)

K = args_list$K
iter = args_list$iter
warmup = args_list$warmup
chains = args_list$chains
thin = args_list$thin
precision = args_list$precision
path_to_save = args_list$path_to_save

model_name = paste0(
  "btblv-precision=", precision, "-",
  "K=", K, ".rds"
)

models_saved = scan("analysis/models/models-saved-list.txt")

list.files("analysis")
#### data ####
data("lf")

lf = lf %>%
  filter(year %in% seq(1950, 2015, 5)) %>%
  filter(!(country %in% c("East Germany", "West Germany", "New Zealand Maori",
                          "New Zealand Non-Maori", "England and Wales (Total Population)",
                          "England and Wales (Civilian Population)",
                          "Scotland", "Northern Ireland", "Wales")))

data = btblv::create_btblv_data(df = lf,
                                resp_col_name = "mx",
                                item_col_name = "age",
                                group_col_name = "country",
                                time_col_name = "year")

fit = btblv_fit(data,
                precision = precision,
                K = K,
                iter = iter,
                warmup = warmup,
                thin = thin,
                chains = chains,
                cores = chains,
                seed = 1)

saveRDS(fit, "models/")


