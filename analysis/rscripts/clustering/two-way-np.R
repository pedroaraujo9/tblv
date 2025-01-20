library(tidyverse)
library(TraMineR)
library(GGally)
library(NbClust)
library(dbscan)
library(mclust)
library(btblv)
library(WeightedCluster)
library(cluster)
library(clusterCrit)
library(MEDseq)
library(ggraph)

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
fit = readRDS("analysis/models/btblv-fit-K=4-clustering_1x1.rds")

post = fit %>% extract_posterior(apply_varimax = T)
summ = post %>% posterior_summary()
conv = check_convergence(post)

conv$beta
conv$sigma
conv$phi
conv$E$rhat %>% summary()
conv$alpha
conv$theta$rhat %>% summary()

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
     
alpha = summ$posterior_mean$alpha                                      
alpha[, 2:3] = -alpha[, 2:3]

post = fit %>% extract_posterior(alpha_reference = alpha, apply_varimax = F)
summ = post %>% posterior_summary()

y = summ$posterior_mean$theta

colnames(y) = c("Elderly", "Adult", "Young", "Old")

plot(summ$posterior_mean$sigma[, 1], summ$posterior_mean$phi[, 1])

plot_latent_effects(summ)

ggpairs(as.data.frame(y))
cor(y)

y %>% apply(MARGIN = 2, FUN = sd)

btblv::plot_latent_effects(summ)


saveRDS(summ, "analysis/results/summ_btblv-fit-K=4-clustering_1x1.rds")
#### finding Z_{it} ####
apply_clust = function(data, G, method) {
  
  set.seed(1)
  
  if(method == "Ward") {
    
    cl_ward = stats::hclust(
      dist(data), 
      method = "ward.D"
    )
    
    class = cutree(cl_ward, k = G)
    
  }else if(method == "K-means") {
    
    class = stats::kmeans(
      data, 
      centers = G, 
      iter.max = 1000, 
      nstart = 10, 
      algorithm = "Hartigan-Wong"
    )
    
    class = class$cluster
    
  }else if(method == "PAM") {
    
    class = cluster::pam(
      x = data, 
      k = G,
      metric = "euclidean"
    )
    
    class = class$clustering
    
  }else if(method == "GMM") {
    class = Mclust(
      data = data, 
      G = G
    )
    
    class = as.integer(class$classification)
  }
  
  return(class)
}

calc_quality = function(data, Gmax, method) {
  
  metrics = purrr::map_df(2:Gmax, ~{
    
    class = apply_clust(data, G = .x, method = method)
    
    intCriteria(
      traj = as.matrix(data), 
      part = class, 
      crit = c("Silhouette", "Calinski_Harabasz", "Point_biserial")
    ) %>% 
      as.data.frame() %>%
      rename(ASW = silhouette, CH = calinski_harabasz, PBC = point_biserial)
    
  }) %>%
    mutate(G = 2:10, method = method)
  
  return(metrics)
}

index_seq_plot = function(data, class) {
  data %>%
    mutate(class = factor(class)) %>%
    ggplot(aes(x=year, y=country, fill=class)) + 
    geom_tile(color="grey") + 
    viridis::scale_fill_viridis(discrete = T)
}

quality = map_df(c("Ward", "K-means", "PAM", "GMM"), ~{
  calc_quality(data = y, Gmax = 10, method = .x)
})

saveRDS(quality, "analysis/results/Z_quality_metrics.rds")

quality %>%
  gather(metric, value, -G, -method) %>%
  ggplot(aes(x=G, y=value, color=method)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = 2:10) + 
  facet_wrap(. ~ metric, scales = "free")

set.seed(1)
Gsel = 4
class_kmeans = apply_clust(y, G = Gsel, method = "K-means")
class_ward = apply_clust(y, G = Gsel, method = "Ward")
class_pam = apply_clust(y, G = Gsel, method = "PAM")
class_gmm = apply_clust(y, G = Gsel, method = "GMM")

clust_scatter = function(data, class) {
  ggpairs(
    data.frame(data), 
    aes(color= factor(class))
  )
}

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

index_seq_plot(ct_df, class_kmeans)
index_seq_plot(ct_df, class_ward)
index_seq_plot(ct_df, class_pam)
index_seq_plot(ct_df, class_gmm)

ct_df$Z = class_kmeans

saveRDS(ct_df, "analysis/results/country_Z.rds")
##### explaning Z #####
ct_df %>%
  left_join(
    mx_tidy %>% select(country, year, age, mx)
  ) %>%
  mutate(log_mx = log(mx)) %>%
  group_by(Z, age) %>%
  summarise(median = median(log_mx), 
            lq = quantile(log_mx, 0.0275), uq = quantile(log_mx, 0.975)) %>%
  ggplot(aes(x=age, y=median, color=factor(Z))) + 
  geom_line() + 
  geom_point() + 
  geom_ribbon(aes(x=age, ymin = lq, ymax = uq, 
                  fill=factor(Z)), alpha = 0.20, inherit.aes = F) #+ 
  #viridis::scale_fill_viridis(discrete = T) + 
  #viridis::scale_color_viridis(discrete = T)

ct_df$Z = factor(ct_df$Z, labels = c("High", "Low", "High+Adult", "Mid"))

z_levels = rev(c("Low", "Mid", "High+Adult", "High"))


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
  geom_bar(position = "fill", width = 1, color="grey80") + 
  viridis::scale_fill_viridis(discrete = T) + 
  labs(x="Year", y="Proportion") + 
  theme_minimal()

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

saveRDS(Hdf, "analysis/results/entropy.rds")

Hdf %>%
  ggplot(aes(x=Period, y=H, group=G, color=G)) + 
  geom_point() + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = T)

#### finding W_i ####
apply_clust_W = function(Z_matrix, Z_dist, G) {
  
  ward_fit = stats::hclust(Z_dist, method = "ward.D")
  ward_class = cutree(ward_fit, k = G)
  
  pam_fit = wcKMedoids(Z_dist, k = G, initialclust = ward_fit)
  pam_class = pam_fit$clustering
  
  medseq_fit = MEDseq_fit(
    seqs = Z_matrix, G = G, 
    weights = NULL, 
    modtype = c("CC", "UC", "CU", "UU"), 
  )
  
  medseq_class = medseq_fit$MAP
  
  res = list(
    ward = list(fit = ward_fit, class = ward_class),
    pam = list(fit = pam_fit, class = pam_class),
    medseq = list(fit = medseq_fit, class = medseq_class)
  )
  
  return(res)
}

calc_W_quality = function(Z_matrix, Z_dist, Gmax) {
  
  metrics = purrr::map_df(2:Gmax, ~{
    
    fit = apply_clust_W(Z_matrix, Z_dist, G = .x)
    
    lapply(names(fit), function(method){
      metrics = wcClusterQuality(Z_dist, factor(fit[[method]]$class))$stats
      metrics[c("HG", "ASW", "CH")] %>%
        rbind() %>%
        as.data.frame() %>%
        mutate(method = method)
    }) %>%
      do.call(rbind, .) %>%
      mutate(G = .x)
  })
  
  rownames(metrics) = NULL
  
  return(metrics)
}

state_couts = seqsubm(Z_seq, method = "TRATE")
state_couts
Z_dist = seqdist(Z_seq, method="OM", sm=state_couts) %>% as.dist()
Z_dist

set.seed(1)
w_quality = calc_W_quality(Z_seq, Z_dist, Gmax = 10)

saveRDS(w_quality, "analysis/results/w_quality.rds")

w_quality %>%
  gather(metric, value, -method, -G) %>%
  ggplot(aes(x=G, y=value, color=method)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ metric, scales = "free") + 
  scale_x_continuous(breaks = 2:10)

medseq_fit = MEDseq_fit(
  seqs = Z_seq, G = 1:10, 
  weights=NULL 
  #modtype = c("CC", "UC", "CU", "UU"), 
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
  scale_x_continuous(breaks = 1:10)

w_fit = apply_clust_W(Z_seq, Z_om, G = 3)

w_fit$medseq
##### ward #### 
plot(w_fit$ward$fit)

country_order = w_fit$ward$fit$labels[w_fit$ward$fit$order]

plot(w_fit$ward$fit)

ggdendro::ggdendrogram(w_fit$ward$fit, segments = T) + 
  theme(text = element_text(size = 20))

ggsave("analysis/plots/W_dgram.pdf", width = 9, height = 6)


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

ggsave("analysis/plots/seq_index_plot.pdf", width = 7, height = 4)

##### pam #####
#Z1 = model.matrix(rep(1, length(w_fit$pam$class)) ~ -1 + factor(w_fit$pam$class)) 
#Z2 = model.matrix(rep(1, length(w_fit$pam$class)) ~ -1 + factor(w_fit$ward$class)) 

pam_class = w_fit$pam$class
names(pam_class) = rownames(Z_matrix)

cl_state_tidy = Z_matrix %>%
  as.data.frame() %>%
  mutate(W = pam_class) %>%
  gather(year, Z, -W) %>%
  mutate(year = as.numeric(year), Z = factor(Z, levels = z_levels))

saveRDS(cl_state_tidy, "analysis/results/W_states_distri.rds")

cl_state_tidy %>%
  ggplot(aes(x=year, fill=Z)) + 
  geom_bar(position = "fill", width = 1, color="black") + 
  viridis::scale_fill_viridis(discrete = T) + 
  labs(x="Year", y="Proportion") + 
  facet_grid(. ~ W) + 
  theme_minimal()

medoids = Z_matrix[unique(pam_class), ]
medoids

medoid_df = medoids %>%
  as.data.frame() %>%
  mutate(medoid = rownames(.)) %>%
  gather(year, Z, -medoid) 

medoid_df %>%
  group_by(medoid, Z) %>%
  summarise(prop = n()/51) %>%
  spread(Z, prop)

medoid_seq = seqdef(medoids, var = colnames(medoids), id = rownames(medoids))
state_couts = seqsubm(medoid_seq, method = "TRATE")
state_couts

med_cl = seqdist(medoid_seq, method = "OM", sm = state_couts) %>%
  as.dist() %>%
  hclust(method = "ward.D")


clust_order = lapply(unique(pam_class), function(x){
  f = pam_class[pam_class == x]
  cl = as.matrix(Z_dist)[names(f), names(f)] %>% 
    as.dist() %>% 
    hclust(method = "ward.D")
  
  cl$labels[cl$order]
})

clust_order = clust_order[c(1, 3, 2)] %>% do.call(c, .)

pam_class[clust_order]

ct_df %>%
  mutate(country = factor(country, levels = clust_order)) %>%
  saveRDS("analysis/results/W_final.rds")

ct_df %>%
  mutate(country = factor(country, levels = clust_order)) %>%
  ggplot(aes(x=year, y=country, fill=factor(Z, levels = z_levels))) + 
  geom_tile(color="grey") + 
  viridis::scale_fill_viridis(discrete = T) + 
  theme_minimal() + 
  labs(x="Period", y="Country", fill=expression(Z[it])) + 
  geom_segment(aes(x = 1960, xend = 2010, y="Canada", yend = "Canada"), 
               position = position_nudge(y=+0.5), color="red") + 
  geom_segment(aes(x = 1960, xend = 2010, y="Slovakia", yend = "Slovakia"), 
               position = position_nudge(y=+0.5), color="red")


seqdist(Z_seq, method = "HAM")

adj_matrix = Z_dist %>% as.matrix()

for(i in rownames(adj_matrix)) {
  for(j in rownames(adj_matrix)) {
    adj_matrix[i, j] = sum(Z_matrix[i, ] == Z_matrix[j, ])
  }
}

for(i in 1:nrow(adj_matrix)) {
  adj_matrix[i, i] = 0
}

g = graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)


V(g)$class = pam_class
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







