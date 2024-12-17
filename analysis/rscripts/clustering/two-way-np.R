library(tidyverse)
library(TraMineR)
library(GGally)
library(NbClust)
library(dbscan)
library(mclust)
library(btblv)

#### data ####
lf = readRDS("analysis/data/life_tables_5x1.rds")

lf = lf %>% filter(year >= 1960, year <= 2010, year %% 1 == 0)

countries = lf %>%
  select(country, year) %>%
  distinct() %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == 51) %>%
  .$country

del_pops = c(
  "England and Wales (Civilian Population)", "England and Wales (Total Population)",
  "Northern Ireland", "Iceland", "West Germany", "East Germany", "Luxembourg", "Scotland"
)

countries = countries[!(countries %in% del_pops)]

lf = lf %>% filter(country %in% countries)

lf$country %>% unique()

mx_tidy = lf %>% 
  filter(country %in% countries) %>% 
  select(country, year, age, mx) %>% 
  as_tibble()

mx = mx_tidy %>% spread(age, mx)
mx_matrix = mx %>% select(-country, -year) %>% as.matrix()


#### dimension reduction ####
X = log(mx_matrix/(1-mx_matrix)) %>% scale()
dec = X %>% cor() %>% eigen()

lam = dec$values
cumsum(lam)/sum(lam)

y = scale(X %*% dec$vectors[, 1:4])

data = btblv::create_btblv_data(
  df = mx_tidy, resp_col_name = "mx", 
  item_col_name = "age", 
  group_col_name = "country", 
  time_col_name = "year"
)

data
plot_corrmatrix(data)
plot_trend(data)

fit = fit_btblv(
  data, 
  K = 4, 
  iter = 2000, 
  warmup = 1000, 
  thin = 5, 
  chains = 3, 
  cores = 3, 
  seed = 1
)

post$post_sample_chains$theta[, , 1,1]

post = fit %>% extract_posterior(apply_varimax = T)
summ = post %>% posterior_summary()
y = summ$posterior_mean$theta

plot(summ$posterior_mean$sigma[, 1], summ$posterior_mean$phi[, 1])

ggpairs(as.data.frame(y))
cor(y)

btblv::plot_latent_effects(summ)

#### finding Z_{it} ####
nb = NbClust(data = y, distance = "euclidean", method = "ward.D")
par(mfrow=c(1, 1))  

Z = nb$Best.partition
Z %>% table() %>% prop.table()

ggpairs(data.frame(y), 
        aes(color= factor(Z)))

Zdf = mx %>%
  select(country, year) %>%
  mutate(Z = factor(Z))

Z_matrix = Zdf %>%
  spread(year, Z) %>%
  select(-country) %>%
  as.matrix()

rownames(Z_matrix) = countries 

#### finding W_i ####
Z_seq = seqdef(Z_matrix, var = colnames(Z_matrix), id = rownames(Z_matrix))

state_couts = seqsubm(Z_seq, method = "TRATE")
Z_om = seqdist(Z_seq, method = "OM", sm = state_couts) %>% as.dist()

W_ward = hclust(Z_om, method = "ward.D")

country_order = W_ward$labels[W_ward$order]

plot(W_ward)

ggdendro::ggdendrogram(W_ward, segments = T) + 
  theme(text = element_text(size = 20))

ggsave("analysis/plots/W_dgram.pdf", width = 9, height = 6)

Zdf %>%
  mutate(country = factor(country, levels = country_order)) %>%
  ggplot(aes(x=year, y=country, fill=Z)) + 
  geom_tile(color="grey") + 
  viridis::scale_fill_viridis(discrete = T) + 
  theme_minimal() + 
  labs(x="Period", y="Country", fill=expression(Z[it]))

ggsave("analysis/plots/seq_index_plot.pdf", width = 7, height = 4)


#### entropy ####
Hdf = lapply(2:10, function(G){
  
  nb = X %>%
    dist() %>%
    hclust(method = "ward.D")
  
  Z = cutree(nb, k = G) %>% factor()
  
  h = mx %>%
    select(country, year) %>%
    mutate(Z = Z) %>%
    spread(year, Z) %>%
    select(-country) %>%
    as.matrix() %>%
    seqdef() %>%
    seqstatd() %>%
    .$Entropy
  
  data.frame(
    Period = mx$year %>% unique(),
    H = h, 
    G = factor(G)
  )
  
}) %>%
  do.call(rbind, .)

Hdf %>%
  ggplot(aes(x=Period, y=H, group=G, color=G)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = T) + 
  theme_minimal()

ggsave("analysis/plots/Entropy-2NP.pdf", width = 6, height = 3.5)

cmdscale(Z_om, k = 3) %>%
  data.frame(x1 = .[, 1], x2 = .[, 2], x3 = .[, 3], country = rownames(.)) %>%
  ggplot(aes(x=x1, y=x3, label=country)) + 
  geom_label() 


#### two-way model based
Mclust


