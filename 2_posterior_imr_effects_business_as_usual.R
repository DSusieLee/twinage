library(tidyverse)
library(readxl)
library(ggdist)
library(hrbrthemes)

cur_country = "Benin"

# reading in # of women by age --------------------------------------------

dt <- read_excel("data/WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx", 
                 sheet = "Medium variant",  
                 skip = 16) %>% 
  select(-c("Index", as.character(c(0:14, 50:99)), "100+")) %>%
  mutate(across(as.character(15:49), as.integer))

dt_women <- dt %>% 
  filter(`Region, subregion, country or area *` %in% cur_country &
           Year %in% c(2023:2053)
  ) %>% 
  rowwise() %>%
  mutate(n = sum(across(as.character(15:49))))

# reading in # of births --------------------------------------------------

dt <- read_excel("data/WPP2022_FERT_F03_BIRTHS_BY_SINGLE_AGE_OF_MOTHER.xlsx", 
                 sheet = "Medium variant", 
                 skip = 16) %>%
  select(-"Index") %>%
  mutate(across(as.character(15:49), as.integer))

dt_births <- dt %>% 
  filter(`Region, subregion, country or area *` %in% cur_country &
           Year %in% c(2023:2053)
  ) %>% 
  rowwise() %>%
  mutate(n = sum(across(as.character(15:49))))

# reading in twin rate ----------------------------------------------------

twindt <- readRDS('./results/stan_outputs/country_twinrates.RDS') %>%
  filter(country %in% cur_country) %>%
  rename(twin_rate = value)

# calculate number of twin deliveries per year ---------------------------------

twin_rate <- twindt$twin_rate
num_births <- dt_births$n*1000

for(i in c(1:31)){
  
  num_twin_deliveries <- 
    num_births[i]*twin_rate/(1 + twin_rate)
  
  twindt <- cbind(twindt, num_twin_deliveries)
  
}

names(twindt) <- c("country", "twin_rate", c(2023:2053))

# calculate number of total deliveries ------------------------------------

twin_rate <- twindt$twin_rate
num_births <- dt_births$n*1000

deliverdt <- twindt[, c(1:2)]

for(i in c(1:31)){
  
  num_total_deliveries <- 
    num_births[i] - num_births[i]*twin_rate/(1 + twin_rate)
  
  deliverdt <- cbind(deliverdt, num_total_deliveries)
  
}

names(deliverdt) <- c("country", "twin_rate", c(2023:2053))

# IMR effect: business as usual; yearly decline by 2 --------------------------------------------------------------

posterior <- readRDS("./results/stan_outputs/stanlm_w_gdp&educ.rds") %>% as.data.frame()

deliverdt$imr <- posterior$imr_scaled*2*(-1) #2 is because yearly change in IMR is declining in 2

deliverdt_long <- pivot_longer(deliverdt, 
                               cols = as.character(c(2023:2053)), 
                               names_to = "Year") %>%
  rename(num_deliveries = value) %>%
  mutate(num_twin_deliveries_baseline = num_deliveries*twin_rate,
         num_twin_deliveries_imr = num_deliveries*(twin_rate + imr),
         sim = rep(c(1:10000), each = 31))

deliverdt_df <- rbind(deliverdt_long %>% 
                        group_by(Year) %>%
                        median_qi(num_twin_deliveries_baseline, .width = c(.50, .80, .95)) %>%
                        rename(twin_deliveries = num_twin_deliveries_baseline) %>%
                        mutate(group = "baseline"),
                      deliverdt_long %>% 
                        group_by(Year) %>%
                        median_qi(num_twin_deliveries_imr, .width = c(.50, .80, .95)) %>%
                        rename(twin_deliveries = num_twin_deliveries_imr) %>%
                        mutate(group = "baseline+imr")) %>%
  mutate(Year = as.numeric(Year))

ggplot(data = 
         deliverdt_df %>% filter(group %in% "baseline" & .width == 0.5), 
       aes(x = Year, y = twin_deliveries, group = 1)) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              fill = "grey",
              alpha = 0.8) +
  geom_ribbon(data = 
                deliverdt_df %>% filter(group %in% "baseline" & .width == 0.8),
              aes(ymin = .lower,
                  ymax = .upper),
              fill = "grey",
              alpha = 0.5) +
  geom_line(size = 1, 
            colour = "black") +
  geom_line(data = deliverdt_df %>% filter(group %in% "baseline+imr" & .width == 0.5),
            size = 1, 
            colour = "black",
            linetype = "dashed") +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  labs(title = paste("Estimated number of twin deliveries - ", cur_country),
       y = "Number of twin deliveries",
       x = "Year") +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10))

## scenario 2: meeting SDG goal of IMR 18.5 -------------------------------------------------------

## scenario 3: reaching Germany level in 20 years (2043) -------------------------------------------------------




