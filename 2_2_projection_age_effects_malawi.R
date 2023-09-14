library(tidyverse)
library(readxl)
library(hrbrthemes)
library(ggdist)
library(tidybayes)

d <- readRDS("./data/ageprop&popsize.rds") %>%
  mutate(num_twin_deliveries_2010 = NA,
         num_twin_deliveries_2040 = NA,
         num_total_deliveries_2010 = NA,
         num_total_deliveries_2040 = NA)

# calculate number of deliveries ------------------------------------------

twindt <- readRDS('./results/stan_outputs/country_twinrates.RDS') %>%
  rename(twin_rate = value)

twin_rate <- twindt$twin_rate
country <- twindt$country
country_list <- unique(country)

for(i in 1:length(country_list)){
  
  cur_country <- country_list[i]
  cur_twin_rate <- twin_rate[country %in% cur_country]
  
  d_filter <- d$country %in% cur_country
  
  num_births2010 <- unique(d$size_2010[d_filter])
  num_births2040 <- unique(d$size_2040[d_filter])
  
  num_twin_deliveries_2010 <- num_births2010*cur_twin_rate/(1 + cur_twin_rate)
  num_twin_deliveries_2040 <- num_births2040*cur_twin_rate/(1 + cur_twin_rate)
  num_total_deliveries_2010 <- num_births2010 - num_twin_deliveries_2010
  num_total_deliveries_2040 <- num_births2040 - num_twin_deliveries_2040
  
  d$num_twin_deliveries_2010[d_filter] <- list(num_twin_deliveries_2010)
  d$num_twin_deliveries_2040[d_filter] <- list(num_twin_deliveries_2040)
  d$num_total_deliveries_2010[d_filter] <- list(num_total_deliveries_2010)
  d$num_total_deliveries_2040[d_filter] <- list(num_total_deliveries_2040)
  
}

# probability of twinning per mab_cat -------------------------------------

predicted <- readRDS("./results/230510_age_effects.rds") %>%
  select(-mab) %>%
  group_by(sim, mab_cat) %>%
  summarise(twin_prob = mean(twin_prob)) #this is relative to the youngest age group

predicted$mab_cat <- rep(unique(d$mab_cat), times = 10000)

predicted <- predicted %>%
  arrange(mab_cat)

# choose country & scenario -----------------------------------------------

##prepare data specific to the country of interest
cur_country <- "Malawi" #adjust
twin_rate <- twindt$twin_rate[twindt$country %in% cur_country]
d_sub <-  d %>% filter(country %in% cur_country)

age_specific_twin_prob <- predicted$twin_prob + rep(twin_rate, times = 7)

##1. actual values: age prop can be either 2010 from WPP or 2010 from DHS
#but I use 2010 from WPP to be consistent with the hypothetical scenarios
age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2010 

dt_actual_values <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                               mab_cat = rep(unique(predicted$mab_cat), each = 10000)
)

k <- 1

for(i in 1:7){
  
  dt_actual_values$num_twin_deliveries[k:(k+10000-1)] <- 
    age_specific_deliveries[, i] * age_specific_twin_prob[k:(k+10000-1)]
  
  k <- k + 10000
  
}

##2. hypothetical values

#2-1. scenario 1: 2010 korea
age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2010_korea 

dt_hypothetical_values1 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                      mab_cat = rep(unique(predicted$mab_cat), each = 10000)
)

k <- 1

for(i in 1:7){
  
  dt_hypothetical_values1$num_twin_deliveries[k:(k+10000-1)] <- 
    age_specific_deliveries[, i] * age_specific_twin_prob[k:(k+10000-1)]
  
  k <- k + 10000
  
}

#2-2. scenario 2: 2040 own country (only age structure)

age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2040

dt_hypothetical_values2 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                      mab_cat = rep(unique(predicted$mab_cat), each = 10000)
)

k <- 1

for(i in 1:7){
  
  dt_hypothetical_values2$num_twin_deliveries[k:(k+10000-1)] <- 
    age_specific_deliveries[, i] * age_specific_twin_prob[k:(k+10000-1)]
  
  k <- k + 10000
  
}

#2-3. scenario 3: 2040 own country (only population size)

age_specific_deliveries <-  d_sub$num_total_deliveries_2040[[1]] %o% d_sub$prop_2010

dt_hypothetical_values3 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                      mab_cat = rep(unique(predicted$mab_cat), each = 10000)
)

k <- 1

for(i in 1:7){
  
  dt_hypothetical_values3$num_twin_deliveries[k:(k+10000-1)] <- 
    age_specific_deliveries[, i] * age_specific_twin_prob[k:(k+10000-1)]
  
  k <- k + 10000
  
}

#2-4. scenario 4: 2040 own country (age structure + population size)

age_specific_deliveries <-  d_sub$num_total_deliveries_2040[[1]] %o% d_sub$prop_2040

dt_hypothetical_values4 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                      mab_cat = rep(unique(predicted$mab_cat), each = 10000)
)

k <- 1

for(i in 1:7){
  
  dt_hypothetical_values4$num_twin_deliveries[k:(k+10000-1)] <- 
    age_specific_deliveries[, i] * age_specific_twin_prob[k:(k+10000-1)]
  
  k <- k + 10000
  
}

# Prepare data for visualization ------------------------------------------

scenarios <- c(paste("Actual:", cur_country, "in 2010"),
               paste("Change 1: Age structure of women giving births in", cur_country, "2040"),
               paste("Change 3: Population size of women giving births in", cur_country, "2040"),
               paste("Change 4: Population size + Age structure of women giving births in", cur_country, "2040"),
               paste("Change 2: Age structure of women giving births in Korea 2010"))

data <- rbind(dt_actual_values %>% mutate(scenario = scenarios[1]),
              dt_hypothetical_values2 %>% mutate(scenario = scenarios[2]),
              dt_hypothetical_values3 %>% mutate(scenario = scenarios[3]),
              dt_hypothetical_values4 %>% mutate(scenario = scenarios[4]),
              dt_hypothetical_values1 %>% mutate(scenario = scenarios[5]))

#summing across age categories within simulation of each scenario
n_scenarios <- length(scenarios)

sum_within_age <- 
  data %>% 
  mutate(sim = rep(rep(c(1:10000), times = 7), times = n_scenarios)) %>%
  group_by(scenario, sim) %>%
  summarise(total_num_twin_deliveries = sum(num_twin_deliveries))

sum_within_age$diff <- sum_within_age$total_num_twin_deliveries - 
  rep(sum_within_age$total_num_twin_deliveries[sum_within_age$scenario %in% scenarios[1]], n_scenarios)

#by scenario
library(gmodels)

sum_within_scenario <- sum_within_age %>% 
  group_by(scenario) %>% 
  summarize(mean = ci(total_num_twin_deliveries)[1],
            lowCI = ci(total_num_twin_deliveries)[2],
            hiCI = ci(total_num_twin_deliveries)[3],
            mean_diff = ci(diff)[1],
            lowCI_diff = ci(diff)[2],
            hiCI_diff = ci(diff)[3]) %>%
  mutate(x = round(mean_diff),
         mean_diff_char = ifelse(x >0, 
                                 paste(as.character(x), "more twin deliveries"),
                                 paste(as.character(x*(-1)), "less twin deliveries"))
  )

sum_within_scenario$label <- 
  c(paste(as.character(round(sum_within_scenario$mean[1])), "twin deliveries"),
    sum_within_scenario$mean_diff_char[2:5])
  
# Visualization -----------------------------------------------------------

x_position <- round(max(data$num_twin_deliveries))*0.67

data %>%
  ggplot(aes(x = num_twin_deliveries, y = mab_cat)) +
  # ggplot(aes(x = num_twin_deliveries, y = reorder(mab_cat, desc(mab_cat)))) +
  expand_limits(x = 0) +
  stat_ccdfinterval(side = "top") +
  # scale_x_continuous(#nigeria
  #   breaks = c(25000, 50000, 75000),
  #   labels = c("25K", "50K", "75K")) +
  # scale_x_continuous(#india
  #   breaks = c(50000, 100000, 150000, 200000),
  #   labels = c("50K", "100K", "150K", "200K")) +
  facet_wrap(~ scenario, ncol = 1) +
  labs(
    y = "Maternal age",
    x = "Number of twin deliveries"
    ) +
  geom_text(
    data = sum_within_scenario %>% select(scenario, label),
    mapping = aes(x = x_position, y = 5.5, label = label),
    size = 5,
    hjust=0
  ) +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  theme(strip.text = element_text(size = 15))

ggsave('./figures/230602_hypothetical_malawi.png', type = "cairo")
ggsave('./figures/230602_hypothetical_nigeria.png', type = "cairo")
ggsave('./figures/230602_hypothetical_india.png', type = "cairo")

# quantifying the difference ----------------------------------------------
  
sum_within_age %>%
  ggplot(aes(x = total_num_twin_deliveries, fill = scenario)) +
  geom_histogram(color="#e9ecef", alpha=0.4, position = 'identity') +
  theme_ipsum() +
  labs(fill="")

sum_within_age %>%
  ggplot(aes(x = total_num_twin_deliveries, y = scenario)) + 
  stat_halfeye()