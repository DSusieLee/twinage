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

# past from WPP -----------------------------------------------------------

wpp_data <- 
  readxl::read_excel("./data/WPP/WPP2022_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                     sheet = "Estimates", #observed values for previous years
                     skip = 16) %>%
  select(-"Index") %>%
  mutate(across(contains("-"), as.integer))

for(i in c(2010, 2020)){
  
  data <- wpp_data %>% 
    filter((tolower(`Region, subregion, country or area *`) %in% country_list) & Year == i) %>%
    select(-c(11,19)) %>%
    rowwise() %>%
    mutate(n = sum(across(c(11:17))),
           across(c(11:17), ~ ./n)) %>%
    ungroup() %>%
    pivot_longer(cols = c(11:17), names_to = "mab_cat") %>%
    rename(country = "Region, subregion, country or area *") %>%
    mutate(
      n = n*1000,
      country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
      country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country))
  
  
  if(i == 2010){dpast <- data}
  else{
    
    dpast <- rbind(dpast, data)
    
  }
  
}

# future from WPP ---------------------------------------------------------

future_years <- c(2050, 2070, 2100)

wpp_data <- 
  readxl::read_excel("./data/WPP/WPP2022_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                     sheet = "Medium variant", #projected values
                     skip = 16) %>%
  select(-"Index") %>%
  mutate(across(contains("-"), as.integer))

for(i in future_years){
  
  data <- wpp_data %>% 
    filter((tolower(`Region, subregion, country or area *`) %in% country_list) & Year == i) %>%
    select(-c(11,19)) %>%
    rowwise() %>%
    mutate(n = sum(across(c(11:17))),
           across(c(11:17), ~ ./n)) %>%
    ungroup() %>%
    pivot_longer(cols = c(11:17), names_to = "mab_cat") %>%
    rename(country = "Region, subregion, country or area *") %>%
    mutate(
      n = n*1000,
      country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
      country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country))
  
  if(i == min(future_years)){dfuture <- data}
  else{
    
    dfuture <- rbind(dfuture, data)
    
  }
  
}

# combine dpast dfuture ---------------------------------------------------

d <- rbind(dfuture, dpast) %>%
  rename(num_total_births = n,
         mab_prop = value) %>%
  select(-c(1, 3:9)) %>%
  mutate(source = "WPP")

# save --------------------------------------------------------------------
saveRDS(d, "./data/projection_data_ageprop&popsize.rds")
