library(tidyverse)
library(readxl)

d <- readRDS("./data/ageprop&popsize.rds") %>%
  mutate(num_twin_deliveries_2010 = NA,
         num_twin_deliveries_2040 = NA,
         num_total_deliveries_2010 = NA,
         num_total_deliveries_2040 = NA,
         pop_diff = size_2040 - size_2010)

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
  summarise(twin_prob = mean(twin_prob)) #age specific twinning propensity BEFORE baseline twinning rate of a country is added

predicted$mab_cat <- rep(unique(d$mab_cat), times = max(predicted$sim))

predicted <- predicted %>%
  arrange(mab_cat)

# all countries -----------------------------------------------------------

scenarios <- c(paste("Actual in 2010"),
               paste("Hypothetical 1: Age structure in 2040"),
               paste("Hypothetical 3: Population size in 2040"),
               paste("Hypothetical 4: Population size + Age structure of in 2040"),
               paste("Hypothetical 2: Age structure of Korea in 2010"))

for(i in 1:length(country_list)){
  
  cur_country <- country_list[i]
  
  twin_rate <- twindt$twin_rate[twindt$country %in% cur_country]
  d_sub <-  d %>% filter(country %in% cur_country)
  
  if(nrow(d_sub) > 0){
    
    age_specific_twin_prob <- predicted$twin_prob + rep(twin_rate, times = 7)
    
    ##1. actual values: age prop can be either 2010 from WPP or 2010 from DHS
    #but I use 2010 from WPP to be consistent with the hypothetical scenarios
    age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2010 
    
    dt_actual_values <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                   mab_cat = rep(unique(predicted$mab_cat), each = 10000)
    )
    
    k <- 1
    
    for(j in 1:7){
      
      dt_actual_values$num_twin_deliveries[k:(k+10000-1)] <- 
        age_specific_deliveries[, j] * age_specific_twin_prob[k:(k+10000-1)]
      
      k <- k + 10000
      
    }
    
    ##2. hypothetical values
    
    #2-1. scenario 1: 2010 korea
    age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2010_korea 
    
    dt_hypothetical_values1 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                          mab_cat = rep(unique(predicted$mab_cat), each = 10000)
    )
    
    k <- 1
    
    for(j in 1:7){
      
      dt_hypothetical_values1$num_twin_deliveries[k:(k+10000-1)] <- 
        age_specific_deliveries[, j] * age_specific_twin_prob[k:(k+10000-1)]
      
      k <- k + 10000
      
    }
    
    #2-2. scenario 2: 2040 own country (only age structure)
    
    age_specific_deliveries <-  d_sub$num_total_deliveries_2010[[1]] %o% d_sub$prop_2040
    
    dt_hypothetical_values2 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                          mab_cat = rep(unique(predicted$mab_cat), each = 10000)
    )
    
    k <- 1
    
    for(j in 1:7){
      
      dt_hypothetical_values2$num_twin_deliveries[k:(k+10000-1)] <- 
        age_specific_deliveries[, j] * age_specific_twin_prob[k:(k+10000-1)]
      
      k <- k + 10000
      
    }
    
    #2-3. scenario 3: 2040 own country (only population size)
    
    age_specific_deliveries <-  d_sub$num_total_deliveries_2040[[1]] %o% d_sub$prop_2010
    
    dt_hypothetical_values3 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                          mab_cat = rep(unique(predicted$mab_cat), each = 10000)
    )
    
    k <- 1
    
    for(j in 1:7){
      
      dt_hypothetical_values3$num_twin_deliveries[k:(k+10000-1)] <- 
        age_specific_deliveries[, j] * age_specific_twin_prob[k:(k+10000-1)]
      
      k <- k + 10000
      
    }
    
    #2-4. scenario 4: 2040 own country (age structure + population size)
    
    age_specific_deliveries <-  d_sub$num_total_deliveries_2040[[1]] %o% d_sub$prop_2040
    
    dt_hypothetical_values4 <- data.frame(num_twin_deliveries = rep(NA, times = 10000*7),
                                          mab_cat = rep(unique(predicted$mab_cat), each = 10000)
    )
    
    k <- 1
    
    for(j in 1:7){
      
      dt_hypothetical_values4$num_twin_deliveries[k:(k+10000-1)] <- 
        age_specific_deliveries[, j] * age_specific_twin_prob[k:(k+10000-1)]
      
      k <- k + 10000
      
    }
    
    #combining into data
    
    if(i == 1){
      
      data <- rbind(dt_actual_values %>% mutate(scenario = scenarios[1]),
                    dt_hypothetical_values2 %>% mutate(scenario = scenarios[2]),
                    dt_hypothetical_values3 %>% mutate(scenario = scenarios[3]),
                    dt_hypothetical_values4 %>% mutate(scenario = scenarios[4]),
                    dt_hypothetical_values1 %>% mutate(scenario = scenarios[5]))
      
      data$country <- cur_country
      
    }
    
    else{
      
      data1 <- rbind(dt_actual_values %>% mutate(scenario = scenarios[1]),
                     dt_hypothetical_values2 %>% mutate(scenario = scenarios[2]),
                     dt_hypothetical_values3 %>% mutate(scenario = scenarios[3]),
                     dt_hypothetical_values4 %>% mutate(scenario = scenarios[4]),
                     dt_hypothetical_values1 %>% mutate(scenario = scenarios[5]))
      
      data1$country <- cur_country
      
      data <- rbind(data, 
                    data1)
      
    } 
    
  }
  
}

# summing up --------------------------------------------------------------

n_scenarios <- length(scenarios)
n_countries <- length(unique(data$country))

sum_within_age <- 
  data %>% 
  mutate(sim = rep(rep(c(1:10000), times = 7), times = n_scenarios*n_countries)) %>%
  group_by(country, scenario, sim) %>%
  summarise(total_num_twin_deliveries = sum(num_twin_deliveries)) %>%
  ungroup()

sum_within_age <- sum_within_age %>%
  group_by(country) %>%
  mutate(baseline = 
           rep(total_num_twin_deliveries[scenario %in% scenarios[1]], times = 5)) %>%
  ungroup()

sum_within_age <- sum_within_age %>%
  mutate(diff = total_num_twin_deliveries - baseline,
         pchange = 100*diff/total_num_twin_deliveries) %>%
  filter(!(country %in% "Tanzania"))

saveRDS(sum_within_age, 
        './results/230602_excess_twins_population_scenarios.rds')

# relative changes (%) in excess twin deliveries  --------------------------------------------------

sum_within_age <- readRDS( './results/230602_excess_twins_population_scenarios.rds')

#by scenario

sum_within_scenario <- sum_within_age %>% 
  group_by(country, scenario) %>% 
  filter(!is.na(diff)) %>%
  summarize(mean = gmodels::ci(pchange)[1],
            lowCI = gmodels::ci(pchange)[2],
            hiCI = gmodels::ci(pchange)[3])

write.csv(sum_within_scenario %>% 
            select(-lowCI) %>% 
            select(-hiCI) %>% 
            pivot_wider(names_from = scenario, values_from = mean), 
          file = './results/230602_excess_twins_population_scenarios_percent.csv')
