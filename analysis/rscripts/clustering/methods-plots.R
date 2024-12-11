library(tidyverse)


ex_index = data.frame(
  Country = rep(c("1", "2", "3"), 5),
  Period = rep(as.character(1:5), each = 3),
  Z = c(
    1, 1, 1,
    1, 1, 2,
    1, 1, 3, 
    2, 2, 3,
    2, 3, 4
  ) %>% as.character()
)

ex_index %>%
  ggplot(aes(x=Period, y=Country, fill=Z, label = Z)) + 
  geom_tile(color="white") + 
  geom_text(color="white") + 
  scale_fill_manual(values = c("grey0", "grey20", "grey40", "grey60")) + 
  labs(fill=expression(Z[it])) + 
  theme_minimal()

ggsave("plots/index_plot_example.pdf")