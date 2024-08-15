library(tidyverse)
library(btblv)
library(patchwork)

str_extract_int_info = function(x, pattern) {
  x %>% 
    stringr::str_extract(pattern) %>%
    stringr::str_extract("\\d{1,10}") %>%
    as.numeric()
}

#### true params ####
sim_data_list = list()
sim_data_list[[2]] = sim_data = readRDS("analysis/data/sim_data_2.rds")
sim_data_list[[4]] = sim_data = readRDS("analysis/data/sim_data_4.rds")

#### models path ####
models_folder = "analysis/models/simulation-study"
models_path = list.files(models_folder) %>% file.path(models_folder, .)
models_path

trueK = models_path %>% str_extract_int_info("trueK=\\d{1,10}-") 
K = models_path %>% str_extract_int_info("-K=\\d{1,10}.rds")
repl = models_path %>% str_extract_int_info("-repl=\\d{1,10}-")

models_path = models_path[trueK == K]
models_path

trueK

sim_study_df = lapply(1:length(models_path), function(i){
  
  print(models_path[i])  
  fit = readRDS(models_path[i])
  
  trueK = models_path[i] %>% str_extract_int_info("trueK=\\d{1,10}-") 
  repl = models_path[i] %>% str_extract_int_info("-repl=\\d{1,10}-")
  
  post_sample = fit$btblv_fit %>% 
    extract_posterior(alpha_reference = sim_data_list[[trueK]]$true_parameters$alpha)
  
  post_summ = post_sample %>% posterior_summary()
  
  post_summ_df = post_summ$posterior_summary_df
  true_summ_df = sim_data_list[[trueK]]$true_parameters_df
  
  post_summ_df = lapply(names(post_summ_df), function(param){
    post_summ_df[[param]]$true_value = true_summ_df[[param]]$mean
    post_summ_df[[param]]$repl = repl
    post_summ_df[[param]]$trueK = trueK
    post_summ_df[[param]]$param = param
    post_summ_df[[param]] 
  }) %>% do.call(bind_rows, .)
  
  post_summ_df
  
}) %>% do.call(bind_rows, .)

sim_study_df %>% saveRDS("analysis/results/sim_study_df.rds")
sim_study_df = readRDS("analysis/results/sim_study_df.rds")

sim_beta_plot = sim_study_df %>%
  filter(param == "beta") %>%
  group_by(trueK, age) %>%
  summarise(avg_est = mean(mean), true_value = mean(true_value)) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=avg_est, y=true_value)) + 
  geom_point() + 
  facet_grid(. ~ trueK) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\beta_{x}$"),
       y= latex2exp::TeX("True value for $\\beta_{x}$"))

sim_beta_plot


sim_beta_plot = sim_study_df %>%
  filter(param == "beta") %>%
  group_by(trueK, age) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=mean, y=true_value)) + 
  geom_point() + 
  facet_grid(. ~ trueK) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Estimates for $\\beta_{x}$"),
       y= latex2exp::TeX("True value for $\\beta_{x}$"))

sim_beta_plot

ggsave("analysis/plots/simulation-study/sim_beta_plot.pdf", width = 5.5, height = 2)

sim_study_df %>%
  filter(param == "beta") %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  select(age, mean, repl, li, ui, trueK) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "beta") %>%
      mutate(trueK = paste0("True K = ", trueK)) %>%
      select(age, true_value, repl, trueK) %>%
      mutate(type="True values") %>%
      rename(mean = true_value) %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=age, y=mean, group=repl, color=type)) + 
  geom_ribbon(aes(x=age, ymin=li, ymax=ui, fill=type, group=repl), alpha=0.02, inherit.aes = F) + 
  geom_point(size=0.7) + 
  facet_wrap(. ~ trueK, scales = "free") + 
  labs(x="Age group", y="Posterior mean \nwith 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top")

ggsave("analysis/plots/simulation-study/sim_beta_hdi.pdf", width = 5, height = 2.5)

sim_study_df %>%
  filter(param == "beta") %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  select(age, mean, repl, li, ui, trueK) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "beta") %>%
      mutate(trueK = paste0("True K = ", trueK)) %>%
      select(age, true_value, repl, trueK) %>%
      mutate(type="True values") %>%
      rename(mean = true_value) %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=age, y=mean, group=repl, color=type)) + 
  geom_jitter(size=0.7) + 
  facet_wrap(. ~ trueK, scales = "free") + 
  labs(x="Age group", y="Posterior mean \nwith 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top")

#### log kappa ####
sim_log_kappa_plot = sim_study_df %>%
  filter(param == "log_kappa") %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=repl, y=mean, ymin=li, ymax=ui)) + 
  geom_point() + 
  geom_errorbar() + 
  geom_hline(aes(yintercept=true_value, color="True value"), linetype="dashed") + 
  facet_wrap(. ~ trueK, scales = "free") + 
  labs(x="Replicate", y=latex2exp::TeX("$\\log(\\kappa)$ posterior mean")) +
  guides(color = "none") + 
  scale_x_continuous(breaks = 1:30, limits = c(1, 30)) + 
  theme(axis.text.x = element_text(size = 4))

sim_log_kappa_plot
ggsave("analysis/plots/simulation-study/sim_log_kappa.pdf", width = 6, height = 2)

sim_beta_plot / sim_log_kappa_plot
ggsave("analysis/plots/simulation-study/sim_beta_log_kappa.pdf", width = 6, height = 4)


#### alpha ####
sim_alpha_plot = sim_study_df %>%
  filter(param == "alpha") %>%
  group_by(trueK, age) %>%
  summarise(avg_est = mean(mean), true_value = mean(true_value)) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=avg_est, y=true_value)) + 
  geom_point() + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\alpha_{xk}$"),
       y= latex2exp::TeX("True value for $\\alpha_{xk}$"))

sim_alpha_plot

sim_alpha_plot = sim_study_df %>%
  filter(param == "alpha") %>%
  group_by(trueK, age) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=mean, y=true_value)) + 
  geom_point() + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Estimates for $\\alpha_{xk}$"),
       y= latex2exp::TeX("True value for $\\alpha_{xk}$"))

sim_alpha_plot
ggsave("analysis/plots/simulation-study/sim_alpha.pdf", width = 5.5, height = 2.2)

# ribbon 
sim_study_df %>%
  filter(param == "alpha", trueK == 2) %>%
  mutate(K = paste0("k = ", K)) %>%
  select(age, mean, repl, K, li, ui) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "alpha", trueK == 2) %>%
      mutate(K = paste0("k = ", K)) %>%
      select(age, true_value, repl, K) %>%
      mutate(type="True values") %>%
      rename(mean = true_value) %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=age, y=mean, group=repl, color=type)) + 
  geom_ribbon(aes(x=age, ymin=li, ymax=ui, fill=type, group=repl), alpha=0.02, inherit.aes = F) + 
  geom_point(size=0.7) + 
  facet_wrap(. ~ K, scales = "free") + 
  labs(x="Age group", y="Posterior mean with\n 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top")

sim_study_df %>%
  filter(param == "alpha", trueK == 4) %>%
  mutate(K = paste0("k = ", K)) %>%
  select(age, mean, repl, K, li, ui) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "alpha", trueK == 4) %>%
      mutate(K = paste0("k = ", K)) %>%
      select(age, true_value, repl, K) %>%
      mutate(type="True values") %>%
      rename(mean = true_value) %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=age, y=mean, group=repl, color=type)) + 
  geom_ribbon(aes(x=age, ymin=li, ymax=ui, fill=type, group=repl), alpha=0.02, inherit.aes = F) + 
  geom_point(size=0.7) + 
  facet_wrap(. ~ K, scales = "free") + 
  labs(x="Age group", y="Posterior mean with\n 95% HPD interval", color="", fill="") + 
  theme(legend.position = "none")


# linerange
jitter_pos = position_jitter(width = 0.2, height = 0) 

sim_study_df %>%
  filter(param == "alpha", trueK == 2) %>%
  mutate(K = paste0("Dimension ", K)) %>%
  select(age, mean, repl, K, li, ui) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "alpha", trueK == 2) %>%
      mutate(K = paste0("Dimension ", K)) %>%
      select(age, true_value, repl, K) %>%
      mutate(type="True values") %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=factor(age), y=mean, color=type)) + 
  geom_jitter(position = jitter_pos, alpha=0.8, size=0.5) + 
  geom_linerange(aes(ymin=li, ymax=ui), position = jitter_pos, alpha=0.2) + 
  geom_point(aes(x=factor(age), y=true_value, color=type), inherit.aes = F, size=0.5) + 
  facet_wrap(. ~ K, scales = "free") + 
  labs(x="Age group", y="Posterior mean with\n 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top") + 
  scale_x_discrete(breaks = seq(0, 110, 20)) + 
  theme(text=element_text(size = 14))
  
ggsave("analysis/plots/simulation-study/sim_alpha_K2_hdi.pdf", width = 7, height = 3)

sim_study_df %>%
  filter(param == "alpha", trueK == 4) %>%
  mutate(K = paste0("Dimension ", K)) %>%
  select(age, mean, repl, K, li, ui) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "alpha", trueK == 4) %>%
      mutate(K = paste0("Dimension ", K)) %>%
      select(age, true_value, repl, K) %>%
      mutate(type="True values") %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=factor(age), y=mean, color=type)) + 
  geom_jitter(position = jitter_pos, alpha=0.8, size=0.5) + 
  geom_linerange(aes(ymin=li, ymax=ui), position = jitter_pos, alpha=0.2) + 
  geom_point(aes(x=factor(age), y=true_value, color=type), inherit.aes = F, size=0.5) + 
  facet_wrap(. ~ K, scales = "free") + 
  labs(x="Age group", y="Posterior mean with\n 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top") + 
  scale_x_discrete(breaks = seq(0, 110, 20)) + 
  theme(legend.position = "none", text=element_text(size = 14))

ggsave("analysis/plots/simulation-study/sim_alpha_K4_hdi.pdf", width = 7, height = 4)

#### theta ####
sim_theta_plot = sim_study_df %>%
  filter(param == "theta") %>%
  group_by(trueK, country, year) %>%
  summarise(avg_est = mean(mean), true_value = mean(true_value)) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=avg_est, y=true_value)) + 
  geom_point() + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\theta_{ik}^{(t)}$"),
       y= latex2exp::TeX("True value for $\\theta_{ik}^{(t)}$"))

sim_theta_plot


sim_theta_plot = sim_study_df %>%
  filter(param == "theta") %>%
  group_by(trueK, country, year) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=mean, y=true_value)) + 
  geom_point() + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Estimates for $\\theta_{ik}^{(t)}$"),
       y= latex2exp::TeX("True value for $\\theta_{ik}^{(t)}$"))

sim_theta_plot

ggsave("analysis/plots/simulation-study/sim_theta.pdf", width = 5.5, height = 2.2)

sim_alpha_plot / sim_theta_plot
ggsave("analysis/plots/simulation-study/sim_alpha_theta.pdf", width = 5.5, height = 4)

#### sigma ####

# point  
sim_sigma_plot = sim_study_df %>%
  filter(param == "sigma") %>%
  group_by(trueK, country, N) %>%
  summarise(avg_est = mean(mean), true_value = mean(true_value)) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=log(avg_est), y=log(true_value), color=factor(N))) + 
  geom_point() + 
  viridis::scale_color_viridis(discrete = T) + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\sigma_{i}$ (on the log scale)"),
       y= latex2exp::TeX("True value for $\\sigma_{i}$ (on the log scale)"),
       color = expression(N[i]))

sim_sigma_plot = sim_study_df %>%
  filter(param == "sigma") %>%
  group_by(trueK, country, N) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=log(mean), y=log(true_value), color=factor(N))) + 
  geom_point() + 
  viridis::scale_color_viridis(discrete = T) + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Estimates for $\\sigma_{i}$ (on the log scale)"),
       y= latex2exp::TeX("True value for $\\sigma_{i}$ (on the log scale)"),
       color = expression(N[i]))

sim_sigma_plot
ggsave("analysis/plots/simulation-study/sim_sigma.pdf", width = 6.5, height = 2.5)



# linerange

jitter_pos = position_jitter(width = 0.2, height = 0) 

sim_study_df %>%
  filter(param == "sigma") %>%
  mutate(trueK = paste0("true K = ", trueK)) %>%
  select(country, mean, repl, trueK, li, ui) %>%
  mutate(type="Estimates") %>%
  bind_rows(
    sim_study_df %>%
      filter(param == "sigma") %>%
      mutate(trueK = paste0("true K = ", trueK)) %>%
      select(country, true_value, repl, trueK) %>%
      mutate(type="True values") %>%
      mutate(repl = -1) %>% 
      distinct()
  ) %>%
  ggplot(aes(x=country, y=mean, color=type)) + 
  geom_jitter(position = jitter_pos, alpha=0.8, size=1.5) + 
  geom_linerange(aes(ymin=li, ymax=ui), position = jitter_pos, alpha=0.1) + 
  geom_point(aes(x=country, y=true_value, color=type), inherit.aes = F, size=2) + 
  labs(x="Country", y="Posterior mean with 95% HPD interval", color="", fill="") + 
  theme(legend.position = "top") + 
  facet_wrap(. ~ trueK, scales = "free") + 
  theme(text=element_text(size = 14)) + 
  coord_flip()

sim_study_df %>%
  filter(param == "sigma") %>%
  mutate(trueK = paste0("true K = ", trueK)) %>%
  select(country, mean, repl, trueK, li, ui, true_value, N) %>%
  ggplot(aes(x=log(mean), y=log(true_value), color=factor(N))) + 
  geom_point() + 
  viridis::scale_color_viridis(discrete = T) + 
  facet_wrap(. ~ trueK, scales="free") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\sigma_{i}$ (on the log scale)"),
       y= latex2exp::TeX("True value for $\\sigma_{i}$ (on the log scale)"),
       color = expression(N[i]))
  
  


#### phi ####
sim_phi_plot = sim_study_df %>%
  filter(param == "phi") %>%
  group_by(trueK, country, N) %>%
  summarise(avg_est = mean(mean), true_value = mean(true_value)) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=logit(avg_est), y=logit(true_value), color=factor(N))) + 
  geom_point() + 
  viridis::scale_color_viridis(discrete = T) + 
  facet_wrap(. ~ trueK, scales="free") + 
  #guides(color = "none") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Average estimate for $\\phi_{i}$ (on the logit scale)"),
       y= latex2exp::TeX("True value for $\\phi_{i}$ (on the logit scale)"),
       color = expression(N[i]))

sim_phi_plot = sim_study_df %>%
  filter(param == "phi") %>%
  group_by(trueK, country, N) %>%
  mutate(trueK = paste0("True K = ", trueK)) %>%
  ggplot(aes(x=logit(mean), y=logit(true_value), color=factor(N))) + 
  geom_point() + 
  viridis::scale_color_viridis(discrete = T) + 
  facet_wrap(. ~ trueK, scales="free") + 
  #guides(color = "none") + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") + 
  labs(x = latex2exp::TeX("Estimates for $\\phi_{i}$ (on the logit scale)"),
       y= latex2exp::TeX("True value for $\\phi_{i}$ (on the logit scale)"),
       color = expression(N[i]))

sim_phi_plot
ggsave("analysis/plots/simulation-study/sim_phi.pdf", width = 6.5, height = 2.5)

sim_sigma_plot / sim_phi_plot
ggsave("analysis/plots/simulation-study/sim_phi_sigma.pdf", width = 5.5, height = 4)
