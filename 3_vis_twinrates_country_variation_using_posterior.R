library(tidyverse)
library(ggridges)
library(viridis)
library(hrbrthemes)

dt <- readRDS("U:/cloud/Projects/twinage/results/stan_outputs/stan_country_only.rds")

# country variation: posterior distributions -------------------------------------------------
data <- dt[, c(1:(ncol(dt)-1))] %>%
  pivot_longer(cols = everything(), names_to = "country") %>%
  mutate(country_bytwinrate = fct_reorder(country, value, .fun = "mean"))

ggplot(data = data, 
       aes(x = value*1000, y = country_bytwinrate, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Twinning rate", 
                       option = "C",
                       breaks = c(5, 10, 15, 20, 25)) +
  scale_y_discrete(expand = c(0,0.5)) +
  labs(title = "Twinning rate by countries",
       subtitle = "posterior distribution of country fixed-effects estimates",
       y = "Country",
       x = "Number of twin deliveries per 1,000 deliveries") +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10))