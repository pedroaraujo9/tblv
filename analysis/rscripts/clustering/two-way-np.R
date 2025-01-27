library(tidyverse)
library(TraMineR)
library(GGally)
library(mclust)
library(btblv)
library(WeightedCluster)
library(cluster)
library(MEDseq)
library(ggraph)
library(dbscan)
source("analysis/rscripts/clustering/utils.R")

#### data ####
lf = readRDS("analysis/data/life_tables_5x1.rds")

lf = lf %>% filter(year >= 1960, year <= 2010, year %% 1 == 0)

countries = lf %>%
  select(country, year) %>%
  distinct() %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == (2010-1960)/1 + 1) %>%
  .$country

countries

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
  iter = 200, 
  warmup = 100, 
  thin = 2, 
  chains = 3, 
  cores = 3, 
  seed = 1, 
  control = list(max_treedepth = 13)
)

#saveRDS(fit, "analysis/models/btblv-fit-K=4-clustering_1x1.rds")
mod = readRDS("analysis/models/btblv-mx-1x1-precision=single-K=4.rds")
fit = mod$btblv_fit
post = fit %>% extract_posterior(apply_varimax = T)
summ = post %>% posterior_summary()
conv = check_convergence(post)

conv$beta
conv$sigma
conv$phi
conv$E$rhat %>% summary()
conv$alpha
conv$theta$rhat %>% summary()
     
alpha = summ$posterior_mean$alpha                                      
alpha[, c(2, 3)] = -alpha[, c(2, 3)]

post = fit %>% extract_posterior(alpha_reference = alpha, apply_varimax = T)
summ = post %>% posterior_summary()

summ$posterior_summary_df$alpha %>%
  ggplot(aes(x=age, y=mean, color=factor(K), fill=factor(K))) + 
  geom_ribbon(aes(x=age, ymin=li, ymax=ui, fill=factor(K)), alpha=0.4, 
              inherit.aes = F) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype=2, alpha=0.8) + 
  scale_x_continuous(breaks = seq(0, 110, 10)) + 
  labs(x = "Age group x", color="Dim", fill="Dim", y=latex2exp::TeX("$\\alpha_{xk}$")) +
  scale_color_manual(values = c("chocolate1", "cornflowerblue", 
                                "darkolivegreen4","deeppink4"
  )) + 
  scale_fill_manual(values = c("chocolate1", "cornflowerblue", 
                               "darkolivegreen4","deeppink4"))

y = summ$posterior_mean$theta

colnames(y) = c("Elderly", "Adult", "Young", "Old")

phi = summ$posterior_mean$phi[, 1]
sigma = summ$posterior_mean$sigma[, 1]
year = mx$year
country = mx$country

data.frame(phi, sigma, country = country %>% unique()) %>%
  ggplot(aes(x=log(sigma), y=log(phi/(1-phi)), label=country)) + 
  geom_label()

plot_latent_effects(summ)

ggpairs(
  data.frame(y, year = mx$year), 
  columns = 1:ncol(y), 
  mapping = aes(alpha = 0.01, color=year),
  upper = list(continuous = wrap("points")) 
) + 
  scale_color_viridis_c(name = "Year")

ggpairs(
  data.frame(y, year = mx$year), 
  columns = 1:ncol(y), 
  mapping = aes(alpha = 0.01) 
)

ggsave("analysis/plots/le-pairs.pdf", width = 8, height = 6.5)
cor(y)

y %>% apply(MARGIN = 2, FUN = sd)

btblv::plot_latent_effects(summ)
data.frame(y, year = mx$year, country = mx$country) %>%
  gather(dim, value, -year, -country) %>%
  ggplot(aes(x=year, y=value, group=country)) + 
  geom_line(alpha = 0.9) + 
  geom_point(size = 0.2) + 
  facet_wrap(. ~ dim, scales = "free") + 
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.8, color = "red")

ggsave("analysis/plots/le-over-time.pdf", width = 7, height = 4)

saveRDS(summ, "analysis/results/summ_btblv-fit-K=4-clustering_1x1.rds")

data.frame(y, year = mx$year, country = mx$country) %>%
  saveRDS("analysis/rscripts/clustering/apps/GMM/data.rds")

#### finding Z_{it} ####
ed = dist(y) %>% as.matrix()

ed[1, country == "Australia"] %>% plot()
ed[1, country == "Russia"] %>% plot()
ed[1, country == "U.S.A."] %>% plot()

colnames(ed) = paste0(country, "_", year)

ed_df = ed %>%
  as.data.frame() %>%
  mutate(ct2 = colnames(.)) %>%
  gather(ct1, d, -ct2) %>%
  as_tibble() %>%
  separate(ct2, into = c("country1", "year1"), sep = "_") %>%
  separate(ct1, into = c("country2", "year2"), sep = "_") %>%
  mutate(year1 = as.integer(year1), year2 = as.integer(year2)) %>%
  filter(!((country1 == country2) & (year1 == year2)), year1 < year2)

ed_df %>%
  filter(country1 == "Australia", year1 == 1960, 
         country2 %in% c("Australia", "Russia", "Belarus", "U.S.A", "U.K.")) %>%
  mutate(year2 = as.integer(year2)) %>%
  ggplot(aes(x=year2, y=d, color=country2, group=country2)) + 
  geom_point() + 
  geom_line()

ed_df %>%
  filter(country1 == "Australia", country2 == "Australia") %>%
  ggplot(aes(x=year2, y=d, group=year1, color=year1)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis()

ed_df %>%
  filter(country1 == "Ireland", country2 == "Ireland") %>%
  ggplot(aes(x=year2, y=d, group=year1, color=year1)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis()

ed_df %>%
  filter(country1 == "Russia", country2 == "Russia") %>%
  ggplot(aes(x=year2, y=d, group=year1, color=year1)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis()

ed_df %>%
  filter(country1 == "U.S.A.", country2 == "U.S.A.") %>%
  ggplot(aes(x=year2, y=d, group=year1, color=year1)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis()

ed_df %>%
  filter(country1 == "Ukraine", country2 == "Ukraine") %>%
  ggplot(aes(x=year2, y=d, group=year1, color=year1)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis()

compute_mahalanobis_pooled = function(x1, x2, n, cov1, cov2) {
  n1 = n2 = n
  pooled_cov = ((n1 - 1) * cov1 + (n2 - 1) * cov2) / (n1 + n2 - 2)
  # Compute Mahalanobis distance
  dist = sqrt(mahalanobis(x1, x2, pooled_cov))
  return(dist)
}

nn = nrow(y)

Dw = matrix(nrow = nn, ncol = nn)
for(i in 1:nn) {
  cat(i, "\r")
  for(j in i:nn) {
    
    h = abs(year[i] - year[j])
    
    phii = phi[which(unique(country) == country[i])]
    phij = phi[which(unique(country) == country[j])]
    sigma2i = sigma[which(unique(country) == country[i])]^2
    sigma2j = sigma[which(unique(country) == country[j])]^2
    
    cphi = (phii + phij)/2
    csigma2 = (sigma2i + sigma2j)/2
    cov = ((cphi^(h)) * (csigma2)/(1-cphi^2)) %>% rep(4) %>% diag()
    Dw[i, j] = Dw[j, i] = mahalanobis(y[i, ], y[j, ], cov)
  }
}

##### Fitting models for Z ######
quality = map_df(c("Ward", "K-means", "PAM", "GMM", "Fuzzy"), ~{
  print(.x)
  calc_quality(
    data_fit = y, 
    data_quality = y, 
    dist_matrix = NULL, 
    Gmax = 10, 
    method = .x
  )
})

saveRDS(quality, "analysis/results/Z_quality_metrics.rds")

quality %>%
  gather(metric, value, -G, -method) %>%
  ggplot(aes(x=G, y=value, color=method)) + 
  geom_point() + 
  geom_line() + 
  labs(y="Metric value", color="Method") +
  scale_x_continuous(breaks = 2:10) + 
  facet_wrap(. ~ metric, scales = "free") + 
  theme(legend.position = "top")

ggsave("analysis/plots/quality-metrics.pdf", width = 7, height = 3)

Gsel = 4
class_kmeans_raw = apply_clust(mx_matrix, G = Gsel, method = "K-means")
class_kmeans_log = apply_clust(log(mx_matrix), G = Gsel, method = "K-means")
class_kmeans = apply_clust(y, G = Gsel, method = "K-means")
class_ward = apply_clust(y, G = Gsel, method = "Ward")
class_pam = apply_clust(y, G = Gsel, method = "PAM")
class_gmm = apply_clust(y, G = Gsel, method = "GMM")
class_fuzzy = apply_clust(y, G = Gsel, method = "Fuzzy")

clust_scatter(y, class_kmeans)
clust_scatter(y, class_ward)
clust_scatter(y, class_pam)
clust_scatter(y, class_gmm)

table(class_kmeans) %>% prop.table()
table(class_ward) %>% prop.table()
table(class_pam) %>% prop.table()
table(class_gmm) %>% prop.table()

ct_df = mx_tidy %>%
  select(country, year) %>%
  distinct()

index_seq_plot(ct_df, class_kmeans_raw)
index_seq_plot(ct_df, class_kmeans_log)
index_seq_plot(ct_df, class_kmeans)
index_seq_plot(ct_df, class_ward)
index_seq_plot(ct_df, class_pam)
index_seq_plot(ct_df, class_fuzzy)
index_seq_plot(ct_df, class_gmm)

ct_df$Z = class_kmeans

Z_matrix_int = ct_df %>%
  select(country, year, Z) %>%
  spread(year, Z) %>%
  select(-country) %>%
  as.matrix()


saveRDS(ct_df, "analysis/results/country_Z.rds")

##### explaning Z #####
Z_centroid = data.frame(y, Z = ct_df$Z) %>% 
  gather(dim, val, -Z) %>%
  group_by(dim, Z) %>%
  summarise(m = mean(val))

Z_centroid %>%
  spread(dim, m) %>%
  select(Z, Young, Adult, Old, Elderly) %>%
  .[c(3, 4, 1, 2),]

Z_centroid %>%
  mutate(Z = factor(Z, levels = z_levels),
         dim = factor(dim, levels = c("Young", "Adult", "Old", "Elderly"))) %>%
  ggplot(aes(x=dim, y=Z, fill=m, label = round(m, 3))) +
  geom_tile() + 
  viridis::scale_fill_viridis()

ct_df$Z = factor(ct_df$Z, labels = c("Mid", "Low", "High", "High+Adult"))
z_levels = rev(c("Low", "Mid", "High+Adult", "High"))

ct_df %>%
  left_join(
    mx_tidy %>% select(country, year, age, mx)
  ) %>%
  mutate(log_mx = log(mx)) %>%
  group_by(Z, age) %>%
  summarise(median = median(log_mx), 
            lq = quantile(log_mx, 0.0275), uq = quantile(log_mx, 0.975)) %>%
  ggplot(aes(x=age, y=median, color=factor(Z, levels = z_levels))) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(x=age, ymin = lq, ymax = uq, 
                  fill=factor(Z, levels = z_levels)), alpha = 0.20, inherit.aes = F) + 
  viridis::scale_fill_viridis(discrete = T) + 
  viridis::scale_color_viridis(discrete = T) + 
  labs(x="Age group", y="Median mortality", color=expression(Z[it]), 
       fill=expression(Z[it]))

ggsave("analysis/plots/cluster-median-mortality.pdf", width = 6, height = 3.5)

Z_matrix = ct_df %>%
  select(country, year, Z) %>%
  spread(year, Z) %>%
  select(-country) %>%
  as.matrix()

rownames(Z_matrix) = ct_df$country %>% unique()
Z_seq = seqdef(Z_matrix, var = colnames(Z_matrix), id = rownames(Z_matrix))

Z_matrix %>% 
  as.data.frame() %>%
  gather(year, Z) %>%
  mutate(Z = factor(Z, levels = z_levels)) %>%
  ggplot(aes(x=as.numeric(year), fill=Z)) + 
  geom_bar(position = "fill", width = 1, color="grey10") + 
  viridis::scale_fill_viridis(discrete = T) + 
  labs(x="Year", y="Proportion", fill=expression(Z[it])) + 
  theme_minimal()

ggsave("analysis/plots/Z-distri.pdf", width = 6, height = 3.5)

#### entropy ####
Hdf = lapply(2:10, function(G){
  
  Z = apply_clust(y, G = G, method = "K-means") %>% factor()
  
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
  labs(y="Normalized Entropy")

ggsave("analysis/plots/Entropy.pdf", width = 6, height = 3.5)

#### finding W_i ####
state_couts = seqsubm(Z_seq, method = "TRATE")
state_couts

centroid_penalty

Z_centroid_matrix = Z_centroid %>% spread(dim, m) %>% as.data.frame()
rownames(Z_centroid_matrix) = Z_centroid_matrix$Z
Z_centroid_matrix = Z_centroid_matrix %>% select(-Z) %>% as.matrix()
centroid_penalty = Z_centroid_matrix %>% dist() %>% as.matrix()


Z_OM_dist = seqdist(Z_seq, method="OM", sm=centroid_penalty) %>% as.dist()
Z_HAM_dist = seqdist(Z_seq, method="HAM") %>% as.dist()

set.seed(1)
w_ham_quality = calc_W_quality(Z_seq, Z_HAM_dist, Gmax = 10)
w_om_quality = calc_W_quality(Z_seq, Z_OM_dist, Gmax = 10)


library(ClickClust)

Z_matrix
Z_list = lapply(1:nrow(Z_matrix), FUN = function(i){
  Z_matrix_int[i, ]
}) %>%
  click.read()

Z_list$X

fit = click.EM(X = Z_list$X, y = Z_list$y, K = 2)

countries[fit$id == 2]
countries[fit$id == 1]


#saveRDS(w_quality, "analysis/results/w_quality.rds")

w_om_quality %>%
  gather(metric, value, -method, -G) %>%
  mutate(method = ifelse(method == "medseq", "MEDseq", method),
         method = ifelse(method == "pam", "PAM", method),
         method = ifelse(method == "ward", "Ward", method)) %>%
  ggplot(aes(x=G, y=value, color=method)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ metric, scales = "free") + 
  scale_x_continuous(breaks = 2:10) + 
  labs(y="Metric value", color="Method: ", x="M (groups)") + 
  theme(legend.position = "top")

w_ham_quality %>%
  gather(metric, value, -method, -G) %>%
  mutate(method = ifelse(method == "medseq", "MEDseq", method),
         method = ifelse(method == "pam", "PAM", method),
         method = ifelse(method == "ward", "Ward", method)) %>%
  ggplot(aes(x=G, y=value, color=method)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ metric, scales = "free") + 
  scale_x_continuous(breaks = 2:10) + 
  labs(y="Metric value", color="Method: ", x="M (groups)") + 
  theme(legend.position = "top")

ggsave("analysis/plots/quality-metrics-W.pdf", width = 7, height = 3)

medseq_fit = MEDseq_fit(
  seqs = Z_seq, G = 1:10, 
  weights=NULL, 
  modtype = c("CC", "UC", "CU", "UU"), 
)

saveRDS(medseq_fit, "analysis/results/medseq_fit.rds")

summary(medseq_fit)


rownames(Z_matrix)[medseq_fit$MAP == 0]
rownames(Z_matrix)[medseq_fit$MAP == 1]
rownames(Z_matrix)[medseq_fit$MAP == 2]

medseq_fit$BIC %>%
  as.data.frame() %>%
  .[1:10, ] %>%
  as.data.frame() %>%
  mutate(G = 1:10) %>%
  gather(model, BIC, -G) %>%
  ggplot(aes(x=G, y=BIC, color=model)) + 
  geom_point() + 
  geom_line() + 
  labs(color="Model", x="M (groups)") +
  scale_x_continuous(breaks = 1:10) 

ggsave("analysis/plots/W-BIC.pdf", width = 6, height = 3.5)


w_fit = apply_clust_W(Z_seq, Z_OM_dist, G = 3)

w_fit$medseq
##### ward #### 
plot(w_fit$ward$fit)

country_order = w_fit$ward$fit$labels[w_fit$ward$fit$order]

plot(w_fit$ward$fit)

ggdendro::ggdendrogram(w_fit$ward$fit, segments = T) + 
  theme(text = element_text(size = 20))

ggsave("analysis/plots/W_dgram.pdf", width = 9, height = 6)

w_fit$ward$class[country_order]

ct_df %>%
  mutate(country = factor(country, levels = country_order)) %>%
  ggplot(aes(x=year, y=country, fill=factor(Z, levels = z_levels))) + 
  geom_tile(color="grey") + 
  viridis::scale_fill_viridis(discrete = T) + 
  theme_minimal() + 
  labs(x="Period", y="Country", fill=expression(Z[it])) + 
  geom_segment(aes(x = 1960, xend = 2010, y="Estonia", yend = "Estonia"), 
               position = position_nudge(y=+0.5), color="red") + 
  geom_segment(aes(x = 1960, xend = 2010, y="Slovakia", yend = "Slovakia"), 
             position = position_nudge(y=+0.5), color="red")

ggsave("analysis/plots/seq-index-plot.pdf", width = 7, height = 4)

##### pam #####
#Z1 = model.matrix(rep(1, length(w_fit$pam$class)) ~ -1 + factor(w_fit$pam$class)) 
#Z2 = model.matrix(rep(1, length(w_fit$pam$class)) ~ -1 + factor(w_fit$ward$class)) 

pam_class = w_fit$ward$class
names(pam_class) = rownames(Z_matrix)

cl_state_tidy = Z_matrix %>%
  as.data.frame() %>%
  mutate(W = pam_class) %>%
  gather(year, Z, -W) %>%
  mutate(year = as.numeric(year), Z = factor(Z, levels = z_levels))

saveRDS(cl_state_tidy, "analysis/results/W_states_distri.rds")

cl_state_tidy %>%
  ggplot(aes(x=year, fill=Z)) + 
  geom_bar(position = "fill", width = 1, color="grey10") + 
  viridis::scale_fill_viridis(discrete = T) + 
  labs(x="Year", y="Proportion") + 
  facet_grid(. ~ W) + 
  theme_minimal() + 
  theme(legend.position = "top")

ggsave("analysis/plots/cluster-prop-states.pdf", width = 8, height = 3)



# Plot the graph using ggraph
ggraph(g, layout = "fr") +  # Fruchterman-Reingold layout
  geom_edge_link(aes(alpha = weight), color = "gray") +  # Edges with alpha based on weight
  geom_node_point(aes(color=factor(class))) +  # Vertices
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  # Add node labels
  scale_edge_alpha_continuous(range = c(0.2, 1)) +  # Adjust alpha range for edges
  theme_minimal() +
  labs(title = "Graph from Adjacency Matrix with Weighted Edges", alpha = "Weight")

# Questions
### Should we smooth theta?
### here we use yearly data (the most common and useful), but in the paper we did 5 years  
### How to choose the best metrics?
### Should we weight the methods? probs from Z, population, etc.
### probability for W would be interesting 







