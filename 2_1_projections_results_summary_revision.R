library(tidyverse)

projection_data <- readRDS('./results/projection_results_240715.rds')

countries <- unique(projection_data$country)

scenarios <- unique(projection_data$scenario)
#[1] "changed_ageprop"      "changed_agestructure" "changed_asfr"        
#[4] "changed_popsize"      "expected_for_2010"    "expected_for_2050"   
#[7] "expected_for_2100" 

metrics <- names(projection_data)[3:6]
#[1] "twin_deliveries" "num_births" "num_deliveries"  "twin_rates" 

# 1. choose scenario & metric ---------------------------------------------------

base_scenario <- scenarios[5]
cur_metric <- metrics[4]

#1-2. Scenario of interest
scenario1 <- scenarios[6]
scenario2 <- scenarios[7]

# 2. Calculate differences ------------------------------------------------

projection_data_subset <- projection_data %>% 
  select(country, scenario, all_of(cur_metric)) %>%
  filter(scenario %in% c(base_scenario, scenario1, scenario2)) %>%
  mutate(pchange = list(NA),
         pchange_mean = NA)

names(projection_data_subset)[3] <- "metric"

country <- projection_data_subset$country
base_scenario_filter <- projection_data_subset$scenario %in% base_scenario
metric <- projection_data_subset$metric

for(i in 1:nrow(projection_data_subset)){
  
  cur_country <- country[i]
  base <- metric[country %in% cur_country & base_scenario_filter]
  pchange <- (unlist(metric[i]) - unlist(base))/unlist(base)
  projection_data_subset$pchange[i] <- list(pchange)
  projection_data_subset$pchange_mean[i] <- mean(pchange)
  
}

#arrange countries
country_arrange <- projection_data_subset %>%
  filter(scenario %in% scenario1) %>%
  arrange(pchange_mean)

projection_data_subset$country1 <- factor(projection_data_subset$country,
                                          levels = country_arrange$country)

# 3. Make a table ------------------------------------------------------------------

cur_dt <- projection_data_subset %>%
  filter(scenario %in% c(scenario1, scenario2)) %>%
  unnest(pchange)

names(cur_dt)[4] <- "value"

cur_dt_country <- cur_dt %>%
  group_by(country, scenario) %>%
  summarise(min = min(value),
            median = median(value),
            mean = mean(value),
            stdev = sd(value),
            q25 = quantile(value, 0.25),
            q75 = quantile(value, 0.75),
            max = max(value)) 
