apply_clust = function(data, dist_matrix = NULL, G, method) {
  
  set.seed(1)
  
  if(method == "Ward") {
    
    if(is.null(dist_matrix)) {
      cl_ward = stats::hclust(
        dist(data), 
        method = "ward.D"
      )
    }else{
      cl_ward = stats::hclust(
        as.dist(dist_matrix), 
        method = "ward.D"
      )
    }
    
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
      G = G,
      hc = TRUE
    )
    
    class = as.integer(class$classification)
  }
  
  return(class)
}

calc_quality = function(data_fit, data_quality, dist_matrix, Gmax, method) {
  
  metrics = purrr::map_df(2:Gmax, ~{
    
    class = apply_clust(data_fit, dist_matrix, G = .x, method = method)
    
    intCriteria(
      traj = as.matrix(data_quality), 
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

clust_scatter = function(data, class) {
  ggpairs(
    data.frame(data), 
    aes(color= factor(class))
  )
}

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
