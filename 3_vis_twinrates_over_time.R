library(tidyverse)
library(haven)
library(hrbrthemes)

#before running this script,
#'b2group' variable needs to be created using '0_create_b2_grouping.R'

cur_dt <- readRDS('./data/dhs.RDS') %>% 
  select(twin, mab, mab_cat, harm_mat_edu, parity, imr, hdi, v010, b0, b2, b2group, sex, subregion, country, gdp_per_capita, motherid) %>%
  filter(#!is.na(mab_cat) & 
    # mab >= 12 &
    #  !is.na(imr) &
    b0 <= 1 & #this makes each row representing one delivery rather than birth
      (subregion %in%  c("Sub-Saharan Africa", "Southern Asia"))) %>%
  mutate(gdp_per_capita = as.numeric(gdp_per_capita),
         mab_scaled = mab - 12, #scale to the minimum maternal age at birth
         imr_scaled = imr - 7.7, #scale to the minimum IMR
         no_prime_educ = harm_mat_edu == 0)

dt <- readRDS("./results/stan_outputs/stan_country_only.rds")

country_data <- dt[, c(1:(ncol(dt)-1))] %>%
  pivot_longer(cols = everything(), names_to = "country") %>%
  mutate(country_bytwinrate = fct_reorder(country, value, .fun = "mean"))

# vis ---------------------------------------------------------------------

# observed twinning rate over time ----------------------------------------

data <- cur_dt %>%
  group_by(country, b2group) %>%
  reframe(
    year = unique(b2),
    twinrate = mean(twin))

data$country_bytwinrate <- factor(data$country, levels = levels(country_data$country_bytwinrate))

ggplot(data %>% filter(!is.na(country_bytwinrate)), 
       aes(x = year,
           y = country_bytwinrate,
           fill = twinrate*1000)) + 
  geom_tile() +
  scale_fill_viridis_c(name = "Twinning rate", 
                       option = "C",
                       breaks = c(5, 10, 15, 20, 25)) +
  labs(title = "Twinning rate over time",
       y = "Country",
       x = "Year") +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10))