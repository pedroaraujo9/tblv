library(tidyverse)
library(latex2exp)
library(patchwork)
library(viridis)

#### model ####
K = 4
precision = "single"
model_fit = readRDS(paste0("analysis/models/btblv-precision=", 
                           precision, "-K=", K, ".rds"))

data = model_fit$btblv_data

set.seed(1)
post_sample = model_fit %>% btblv::extract_posterior(
  alpha_reference = "pca", apply_varimax = TRUE
)

post_summ = post_sample %>% btblv::posterior_summary() 

alpha = post_summ$posterior_mean$alpha

alpha[, 1] %>% plot()
alpha[, 2] %>% plot()
alpha[, 3] %>% plot()
alpha[, 4] %>% plot()

alpha[, 3] = -alpha[, 3]

post_sample = model_fit %>% btblv::extract_posterior(
  alpha_reference = alpha, apply_varimax = FALSE
)

post_summ = post_sample %>% btblv::posterior_summary() 

#### beta ####
bp = post_summ$posterior_summary_df$beta %>%
  ggplot(aes(x=age, y=mean)) + 
  geom_ribbon(aes(x=age, ymin=li, ymax=ui), alpha = 0.4) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 110, 10)) + 
  labs(x = "Age group x", y= latex2exp::TeX("$\\beta_{x}$"))

bp

post_summ$posterior_summary_df$log_kappa %>% round(3)

#### alpha ####
ap = post_summ$posterior_summary_df$alpha %>%
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
                               "darkolivegreen4","deeppink4")) + 
  theme(text = element_text(size = 15))

ap
ggsave("analysis/plots/alpha_est.pdf", width = 6, height = 3.5)

#### alpha and beta ####
theta_max = lapply(1:K, function(k){
  theta = post_summ$posterior_mean$theta
  theta[which.max(theta[, k]), ]
}) %>%
  do.call(rbind, .) %>%
  diag() %>%
  diag() %>%
  rbind(
    rep(0, K)
  ) 

mu = btblv::inv_logit(theta_max%*%t(post_summ$posterior_mean$alpha) + cbind(rep(1, nrow(theta_max))) %x% t(post_summ$posterior_mean$beta))

mu_df = mu %>% 
  as.data.frame() 

names(mu_df) = paste0("V", model_fit$btblv_data$df$age %>% unique())

mu_df = mu_df %>%
  mutate(dims = c(1:nrow(theta_max))) %>%
  gather(age, mu, -dims) %>%
  mutate(age = age %>% str_extract("\\d{1,10}") %>% as.integer(),
         dims = factor(dims))

example_log_mortality = mu_df %>% 
  ggplot(aes(x=age, y=btblv::logit(mu), color=dims)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 110, 10)) + 
  scale_color_manual(values = c("chocolate1", "cornflowerblue", 
                                "darkolivegreen4","deeppink4", "black"
  ),
  labels = unname(c(
    latex2exp::TeX("Max $\\theta_{i1}^{(t)}$"),
    latex2exp::TeX("Max $\\theta_{i2}^{(t)}$"),
    latex2exp::TeX("Max $\\theta_{i3}^{(t)}$"),
    latex2exp::TeX("Max $\\theta_{i4}^{(t)}$"),
    latex2exp::TeX("$\\beta_{x}$")
  ))) +
  labs(x="Age group x", y=latex2exp::TeX("logit$(\\mu_{xit})$"), color="") 

example_log_mortality
ggsave("analysis/plots/mortality_example.pdf", width = 6, height = 3.5)


ap + (example_log_mortality + theme(text = element_text(size = 15)))
ggsave("analysis/plots/alpha_mortality_example.pdf", width = 12, height = 3.5)


ap / (example_log_mortality + theme(text = element_text(size = 15)))
ggsave("analysis/plots/alpha_logit_curve.pdf", width = 6, height = 6)

post_summ$posterior_summary_df$log_kappa

#### theta ####
theta_wide = post_summ$posterior_summary_df$theta %>%
  select(mean, country, year, K) %>%
  spread(K, mean)

select_countries = c("Ireland", "Ukraine", "Japan")

post_summ$posterior_summary_df$theta %>%
  mutate(sel_country = ifelse(country %in% select_countries, country, "Other")) %>%
  mutate(sel_alpha = ifelse(sel_country == "Other", "other", "Sel")) %>%
  mutate(ribbon_country = ifelse(country %in% select_countries, country, NA)) %>%
  mutate(K = paste0("Dimension ", K)) %>%
  ggplot(aes(x=year, y=mean, group=country, color=sel_country, alpha = sel_alpha)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_alpha_manual(values = c(0.16, 1), guide = "none") + 
  scale_color_manual(values = c("chartreuse3", "red", "black", "blue")) + 
  scale_fill_manual(values = c("chartreuse3", "red", "black", "blue", "grey")) + 
  facet_wrap(K ~ ., scales = "free_y") + 
  labs(x="Year", y=latex2exp::TeX("$\\theta_{ik}^{(t)}$"), color="Country:") +
  theme(legend.position = "top", text = element_text(size = 9))

ggsave("analysis/plots/theta_time.pdf", width = 5, height = 3.5)

##### AR(1) params ####
ar_params = bind_rows(
  post_summ$posterior_summary_df$phi %>%
    mutate(param = "phi") %>%
    arrange(N),
  
  post_summ$posterior_summary_df$sigma %>%
    mutate(param = "sigma") %>%
    arrange(N)
) %>%
  mutate(param = factor(param, levels = c("phi", "sigma")),
         country = factor(country, levels = unique(.$country)))


ar_params$country

levels(ar_params$param) = c(
  "phi" = latex2exp::TeX("$\\phi_{i}$"),
  "sigma" = latex2exp::TeX("$\\sigma_{i}$")
)

ar_params %>%
  ggplot(aes(x=country, y=mean, color=N)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=li, ymax=ui)) + 
  viridis::scale_color_viridis() + 
  facet_wrap(. ~ param, scales = "free", labeller = label_parsed) + 
  coord_flip() +
  labs(y="Posterior", x="Country")

ggsave("analysis/plots/sigma_phi_hdi.pdf", width = 8, height = 5.5)

data.frame(
  phi = post_summ$posterior_summary_df$phi$mean, 
  sigma = post_summ$posterior_summary_df$sigma$mean,
  N = post_summ$posterior_summary_df$sigma$N,
  country = post_summ$posterior_summary_df$sigma$country
) %>%
  mutate(label_color = ifelse(N <= 7, T, F)) %>%
  ggplot(aes(x=log(sigma), y=btblv::logit(phi), fill=factor(N), label=country, color=label_color)) + 
  geom_label(size = 2.5) + 
  guides(color = "none") +
  viridis::scale_fill_viridis(discrete = T) + 
  scale_color_manual(values = c("black", "white")) + 
  labs(x = expression(log(sigma[i])), y=expression(logit(phi[i])), 
       fill=expression(N[i])) + 
  scale_y_continuous(labels = paste0("logit(", btblv::inv_logit(c(1, 2, 3, 4)) %>% round(2), ")")) +
  scale_x_continuous(labels = paste0("log(", exp(c(-2.25, -2.00, -1.75, -1.5, -1.25)) %>% round(2), ")"))

ggsave("analysis/plots/sigma_phi_est.pdf", width = 5.5, height = 3)

ar_params %>% filter(country == "Ireland")
