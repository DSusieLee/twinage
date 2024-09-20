library(tidyverse)
library(haven)

cur_dt <- readRDS('./data/dhs_past10yrs.RDS') %>%
  select(b0, mab,subregion, country) %>%
  mutate(mab_cat = cut(mab, c(14, seq(19, 44, by= 5), 49))) %>%
  filter(b0 <= 1 & #this makes each row representing one delivery rather than birth
           (subregion %in%  c("Sub-Saharan Africa", "Southern Asia")) &
           (!country %in% "South Africa"))  

country_list_data <- unique(cur_dt$country)

mab_category <- levels(cur_dt$mab_cat)

d <- data.frame(mab_cat = rep(mab_category, times = length(country_list_data)),
                country = rep(country_list_data, each = length(mab_category)))

country_list <- c(tolower(unique(country_list_data)),
                  "côte d'ivoire",
                  "congo")

past_years <- c(2010,2020)
future_years <- c(2050, 2070, 2100)

# 1-1. Popsize  -----------------------------------------------------------

#past
dpast <- 
  readxl::read_excel("./data/WPP/WPP2022_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx", 
                     sheet = "Estimates", #observed values for previous years
                     skip = 16) %>%
  rename(country = "Region, subregion, country or area *") %>%
  mutate(country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
         country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country)) %>%
  filter((tolower(country) %in% country_list),
         Year %in% past_years) %>%
  select(country, Year, c(15:21)) %>%
  pivot_longer(cols = c(3:9), names_to = "mab_cat") 

#future

dfuture <- 
  readxl::read_excel("./data/WPP/WPP2022_POP_F02_3_POPULATION_5-YEAR_AGE_GROUPS_FEMALE.xlsx", 
                     sheet = "Medium variant", #projected values
                     skip = 16) %>%
  rename(country = "Region, subregion, country or area *") %>%
  mutate(country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
         country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country)) %>%
  filter((tolower(country) %in% country_list),
         Year %in% future_years) %>%
  select(country, Year, c(15:21)) %>%
  pivot_longer(cols = c(3:9), names_to = "mab_cat") 

#combine
d1 <- rbind(dpast, dfuture) %>%
  mutate(value = as.numeric(value)) %>%
  rename(age_structure = value) %>%
  arrange(country, Year) %>%
  reframe(popsize = sum(age_structure), .by = c(country, Year), across()) %>%
  mutate(age_prop = age_structure/popsize)

# 1-2. ASFR -----------------------------------------------------------

#past
dpast <- 
  readxl::read_excel("./data/WPP/WPP2022_FERT_F02_FERTILITY_RATES_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                     sheet = "Estimates", #observed values for previous years
                     skip = 16) %>%
  rename(country = "Region, subregion, country or area *") %>%
  mutate(country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
         country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country)) %>%
  filter((tolower(country) %in% country_list),
         Year %in% past_years) %>%
  select(country, Year, c(13:19)) %>%
  pivot_longer(cols = c(3:9), names_to = "mab_cat") 

#future  
dfuture <- 
  readxl::read_excel("./data/WPP/WPP2022_FERT_F02_FERTILITY_RATES_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                     sheet = "Medium variant", #projected values
                     skip = 16) %>%
  rename(country = "Region, subregion, country or area *") %>%
  mutate(country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
         country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country)) %>%
  filter((tolower(country) %in% country_list),
         Year %in% future_years) %>%
  select(country, Year, c(13:19)) %>%
  pivot_longer(cols = c(3:9), names_to = "mab_cat") 

#combine
d2 <- rbind(dpast, dfuture) %>%
  mutate(value = as.numeric(value)/1000) %>%
  rename(asfr = value) %>%
  arrange(country, Year)

# 2. Number of births by scenarios -----------------------------------------------------

##changes in twinning rates
scenario_1 <- left_join(d1 %>% filter(Year == 2010) %>% select(-Year),
                        d2 %>% filter(Year == 2100) %>% select(-Year),
                        by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "changed_asfr")

scenario_2 <- left_join(d1 %>% filter(Year == 2100) %>% select(-Year),
                        d2 %>% filter(Year == 2010) %>% select(-Year),
                        by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "changed_agestructure")

##changes in number of births
scenario_3 <- left_join(d1 %>% filter(Year == 2010) %>% select(popsize, country, mab_cat, -Year),
                        d1 %>% filter(Year == 2100) %>% select(age_prop, country, mab_cat, -Year),
                        by = c("country","mab_cat")) %>%
  mutate(age_structure = popsize*age_prop) %>%
  left_join(d2 %>% filter(Year == 2010) %>% select(-Year),
            by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "changed_ageprop")

scenario_4 <- left_join(d1 %>% filter(Year == 2100) %>% select(popsize, country, mab_cat, -Year),
                        d1 %>% filter(Year == 2010) %>% select(age_prop, country, mab_cat, -Year),
                        by = c("country","mab_cat")) %>%
  mutate(age_structure = popsize*age_prop) %>%
  left_join(d2 %>% filter(Year == 2010) %>% select(-Year),
            by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "changed_popsize")

##everything as expected

scenario_5 <- left_join(d1 %>% filter(Year == 2010) %>% select(-Year),
                        d2 %>% filter(Year == 2010) %>% select(-Year),
                        by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "expected_for_2010")

scenario_6 <- left_join(d1 %>% filter(Year == 2100) %>% select(-Year),
                        d2 %>% filter(Year == 2100) %>% select(-Year),
                        by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "expected_for_2100")

scenario_7 <- left_join(d1 %>% filter(Year == 2050) %>% select(-Year),
                        d2 %>% filter(Year == 2050) %>% select(-Year),
                        by = c("country","mab_cat")) %>%
  mutate(num_births = age_structure*asfr,
         scenario = "expected_for_2050")

##combine data across scenarios

d <- rbind(scenario_1,
           scenario_2,
           scenario_3,
           scenario_4,
           scenario_5,
           scenario_6,
           scenario_7) %>%
  arrange(country,scenario,mab_cat) %>%
  relocate(age_prop, .before = age_structure) %>%
  relocate(mab_cat, .after = country) %>%
  relocate(scenario, .after = mab_cat)

countries_for_projections <- unique(d$country)

# 3. Prepare for calculating num twin deliveries --------------------------

twindt <- readRDS('./results/stan_outputs/country_twinrates.RDS') %>%
  rename(twin_rate = value)

countries_for_projections <- intersect(unique(twindt$country), countries_for_projections)

mabcategory <- unique(d$mab_cat)

###twinning propensity by maternal age
post <- readRDS("./results/stan_outputs/stanlm_mab_cat_only.rds")
posterior <- as.data.frame(post)
#intercept: youngest MAB group in Afghanistan
#if "post <- readRDS("./results/stan_outputs/stanlm_mab_cat.rds")"
#then intercept is parity 1 youngest MAB group in Afghan. Results are more or less the same. 

posterior <- posterior[,c(1:7)]
names(posterior) <- mabcategory

posterior <- pivot_longer(posterior, cols = names(posterior)) %>%
  rename(mab_cat = name,
         twin_prob = value) %>%
  arrange(mab_cat)

#to the 'posterior' data (age specific twinning probability in Afghanistan), 
#adding the "difference" between average twinning rates of a country relative to average Afghanistan
#will give the age specific twinning probability for that country

country_posterior <- readRDS("./results/stan_outputs/stan_country_only.rds")

afghan <- country_posterior %>% select(1)

# 4. Calculate number of twin deliveries -----------------------

d$num_twin_deliveries <- list(NA)

for(i in 1:length(countries_for_projections)){
  
  cur_country <- countries_for_projections[i]
  
  ##age-specific twinning probability (10000 values for each 7 maternal age categories)
  
  if(cur_country %in% "Afghanistan"){
    
    age_specific_twin_prob <- posterior
    
  }
  
  else{
    
    
    age_specific_twin_prob <- posterior
    
    x <- age_specific_twin_prob$twin_prob +
      do.call("rbind", 
              replicate(7, 
                        country_posterior %>% select(all_of(cur_country)) - afghan,
                        simplify = FALSE))
    
    age_specific_twin_prob$twin_prob <- x[,1]
    
  }
  
  ##age-specific twin deliveries
  
  for(j in 1:length(mabcategory)){
    
    #the length of this should be the number of scenarios
    cur_ix <- which((d$country %in% cur_country) & (d$mab_cat %in% mabcategory[j]))
    cur_ix_length <- length(cur_ix)
    
    #the nrow of this should always be 10000; same across different scenarios and only differs by country and mab category
    cur_age_specific_twin_prob <- age_specific_twin_prob %>% filter(mab_cat %in% mabcategory[j])
    
    for(k in 1:cur_ix_length){#going through each value of num_births based on different scenarios
      
      num_twin_deliveries <- (d$num_births[cur_ix[k]] * cur_age_specific_twin_prob$twin_prob) / (1 + cur_age_specific_twin_prob$twin_prob)
      
      d$num_twin_deliveries[cur_ix[k]] <- list(num_twin_deliveries)
      
    }
    
  }
  
}


# 5. twin rates by scenario -----------------------------------------------

d_summary <- d %>% 
  group_by(country, scenario) %>%
  reframe(twin_deliveries = list(mapply(FUN = sum,
                                        num_twin_deliveries[[1]],
                                        num_twin_deliveries[[2]],
                                        num_twin_deliveries[[3]],
                                        num_twin_deliveries[[4]],
                                        num_twin_deliveries[[5]],
                                        num_twin_deliveries[[6]],
                                        num_twin_deliveries[[7]])),
          num_births = sum(num_births)) %>%
  mutate(num_deliveries = list(NA),
         twin_rates = list(NA))

for(i in 1:nrow(d_summary)){
  
  twin_deliveries <- unlist(d_summary$twin_deliveries[i])
  n_deliveries <- d_summary$num_births[i] - twin_deliveries
  twin_rates <- twin_deliveries/n_deliveries
  
  d_summary$num_deliveries[i] <- list(n_deliveries)
  d_summary$twin_rates[i] <- list(twin_rates)
  
}

# 6. save ----------------------------------------------------------

saveRDS(d_summary, './results/projection_results_240715.rds')
