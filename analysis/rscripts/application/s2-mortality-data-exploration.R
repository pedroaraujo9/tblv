library(btblv)
library(tidyverse)
library(viridis)
library(patchwork)
library(EnvStats)


#### HMD data from package btblv ####
lf = readRDS("analysis/data/model_data_incomplete.rds")

life_tables = lf %>%
  mutate(ind = paste0(country, "-", year)) %>%
  filter(age < 110)

life_tables$mx %>% summary()
life_tables$qx %>% summary()

#### data availability ####
expand.grid(
  country = life_tables$country %>% unique(),
  year = life_tables$year %>% unique()
) %>%
  as_tibble() %>%
  left_join(
    life_tables %>%
      select(year, country) %>%
      distinct() %>%
      mutate(available = T)
  ) %>%
  mutate(available = ifelse(is.na(available), F, T)) %>%
  ggplot(aes(x=country, y=year, fill=available)) +
  geom_tile(color="white") +
  scale_y_continuous(breaks = seq(1950, 2015, 5)) +
  labs(x="Country", y="Year", fill="Data available") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.3))

ggsave("analysis/plots/mortality_data_availability.pdf", width = 8, height = 4)

#### some mortality curves ####
life_tables %>%
  filter(year %in% c(1950, 1980, 2000, 2015)) %>%
  ggplot(aes(x=age, y=logit(qx), group=ind, color=factor(year))) +
  geom_line(alpha=0.4) +
  scale_color_viridis(discrete = T) +
  labs(x="Age group", y=expression(logit(q[xit])), color="Year")

ggsave("analysis/plots/mortality_curves_original_log_scale.pdf", width = 4.5, height = 2.5)

life_tables %>%
  filter(year %in% c(1950, 1980, 2000, 2015)) %>%
  ggplot(aes(x=age, y=qx, group=ind, color=factor(year))) +
  geom_line(alpha=0.4) +
  scale_color_viridis(discrete = T) +
  labs(x="Age group", y=expression(q[xit]), color="Year")

ggsave("analysis/plots/mortality_curves_original_scale.pdf", width = 4.5, height = 2.5)

life_tables %>%
  filter(country=="Ireland") %>%
  ggplot(aes(x=age, y=mx, group=ind, color=year)) +
  geom_line() +
  labs(x="Age group", y=expression(q[xit]), color="Year:") +
  scale_color_viridis()  +
  theme(text = element_text(size = 12))

ggsave("analysis/plots/ireland_mortality_curves.pdf", width = 4.5, height = 2.5)

#### correlation matrix ####
life_tables %>%
  select(country, year, age, qx) %>%
  filter(age < 110) %>%
  spread(age, qx) %>%
  select(-country, -year) %>%
  as.matrix() %>%
  logit() %>%
  cor() %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(age1 = colnames(.)) %>%
  gather(age2, value, -age1) %>%
  filter(as.numeric(age1) < as.numeric(age2), age1 != age2) %>%
  mutate(value = as.numeric(value),
         age1 = factor(age1, levels = life_tables$age %>% unique()),
         age2 = factor(age2, levels = life_tables$age %>% unique())) %>%
  mutate(color_label = ifelse(value <= 0.60, T, F)) %>%
  ggplot(aes(x=age1, y=age2, fill=value, label = round(value, 2))) +
  geom_tile(color="white") +
  #geom_text(aes(color=color_label), size = 1.5) +
  viridis::scale_fill_viridis() +
  guides(color = "none") +
  scale_color_manual(values = c("black", "white")) +
  labs(x="Age group", y="Age group", fill="Corr") + 
  theme_minimal()

ggsave("analysis/plots/correlation_matrix.pdf", width = 6.5, height = 4.5)


#### trend analysis ####
get_tau = function(mx) {
  tau = EnvStats::kendallTrendTest(mx, ci.slope=F)
  tau$estimate["tau"]
}

tau_over_time = life_tables %>%
  select(country, age, year, qx) %>%
  filter(age < 110) %>%
  arrange(country, year, age) %>%
  group_by(country, age) %>%
  summarise(tau = get_tau(qx))

tau_over_time %>%
  ggplot(aes(x=factor(age), y=country, fill=tau)) +
  geom_tile(color="black") +
  scale_fill_viridis() +
  labs(x="Age group", y="Country", fill=expression(tau))

ggsave("analysis/plots/tau_trend.pdf", width = 8, height = 6.5)

