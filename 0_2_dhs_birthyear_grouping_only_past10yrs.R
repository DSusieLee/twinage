library(tidyverse)
library(haven)
library(hrbrthemes)
library(lubridate)

dt <- readRDS('./data/dhs.RDS') %>%
  filter(
      !is.na(b0) & #whether or not multiple births
      between(mab, 15, 49)) %>% 
  filter(b0 <= 1) %>% #this makes each row representing one delivery rather than birth
  select(-year_child_birth_group) %>%
  filter((year_survey - year_child_birth) <= 10) %>% #live births within the 10 years preceding the survey 
  filter(subregion %in% c("Sub-Saharan Africa", "Southern Asia")) %>%
  filter(! country %in% c("Sao Tome & Principe", "Eswatini", "Sri Lanka")) 

#if doing by zone, then
# %>%
#   select(-country) %>%
#   rename(country = zone)

# b2_grouping -------------------------------------------------------------

uniqcountry <- unique(dt$country)

# 1. What is the minimum observations needed to detect twinning? ----------

plot(seq(1000, 5000, by = 100),
     1 - pbinom(50, size = seq(1000, 5000, by = 100), prob = 17/1000))

min_obs <- 4000

# 2. create country-specific birth year (b2) bins -------------------------

options(warn=2)

percountry <- dt %>%
  group_by(country, year_child_birth) %>%
  count()

percountry$b2sum <- rep(NA, nrow(percountry))
percountry$b2group <- rep(NA, nrow(percountry))

for(i in 1:length(uniqcountry)){
  
  ix <- percountry$country %in% uniqcountry[i]
  
  curdt <- percountry[ix, ]
  l <- nrow(curdt)
  
  b2sum <- cumsum(curdt$n)
  b2group <- rep(1, nrow(curdt))
  
  min_obs_start <- b2sum[min(which(b2sum >= min_obs))]
  b2group[(which(b2sum==min_obs_start)+1):l] <- 2
  
  k <- 3
  
  for(j in (which(b2sum==min_obs_start)+1):l){
    
    if(j < l){
      
      if((b2sum[j]-min_obs_start) >= min_obs){
        
        b2group[(j+1):l] <- k
        
        k <- k+1
        min_obs_start <- b2sum[j]}
      
    }
    
    else{
      
      b2group[l] <- k-1
      
    }
    
  }
  
  percountry$b2sum[ix] <- b2sum
  percountry$b2group[ix] <- b2group
  
}


# 3. For birth year groups with less than min_obs --------

perb2group <- percountry %>%
  group_by(country, b2group) %>%
  summarize(obs = sum(n))

obs_num <- perb2group$obs
obs_not_enough <- obs_num < min_obs

b2group <- perb2group$b2group
b2group_update <- b2group

ix <- which(obs_not_enough)

for(i in ix){
  
  b2group_update[i] <- b2group[i-1]
  
}

perb2group$b2group_update <- b2group_update

percountry <- left_join(percountry,
                        perb2group %>% select(-obs),
                        by = c("country","b2group")) %>%
  select(-b2group) %>%
  rename(b2group = b2group_update)

# 4. assign the country-specific bins to the dt data ---------------------

dt <- left_join(dt, 
                percountry %>% select(country, year_child_birth, b2group), 
                by = c("country", "year_child_birth"))

dt <- dt %>%
  rename(year_child_birth_group = b2group)

# -------------------------------------------------------------------------

saveRDS(dt, './data/dhs_past10yrs.rds')

