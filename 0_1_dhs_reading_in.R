library(haven)
library(tidyverse)
library(readxl)

# data from Kieron --------------------------------------------------------

#1. initial reading
dhs <- subset(read_dta("data/wfs_dhs_ffs_ggs_2020_05.dta"),
              data_source == "dhs")
saveRDS(dhs, './data/dt_dhs.RDS')

wfs <- subset(read_dta("data/wfs_dhs_ffs_ggs_2020_05.dta"),
              data_source == "wfs")
saveRDS(wfs, './data/dt_wfs.RDS')

#2. processing only the dhs
dhs1 <- readRDS('./data/dt_dhs.RDS') %>%
  select(-c(26:35), -c(37:42), harm_mat_edu, e0, u5mr, gdp, gdp_growth, tfr) %>%
  mutate(twin = b0>0, #1 = non-singleton, 0 = singleton
         v010 = ifelse(v010<100, v010 + 1900, v010), #v010 = year at mother's birth
         b2 = ifelse(b2<100, b2 + 1900, b2),
         mab = b2 - v010, #b2 = year at child's birth,
         mab_cat = cut(mab, c(min(mab)-1, 19, 24, 29, 34, 39, 44, max(mab))),
         motherid = paste(country_string, motherid),
         mab_cat_num = as.numeric(mab_cat)) %>%
  rename(parity = bord, #bord = birth order
         country = country_string, 
         sex = b4) %>%
  filter(! is.na(b2)) %>%
  select(-c("cc_numeric","setsize","caseid","maternal_education","mother_birthyear","true_birthorder","death_year","death_month"))

# data added later --------------------------------------------------------

dhs2 <- readRDS("./data/dt_dhs_new_220427.RDS") %>%
  rename(motherid = motherID) %>%
  mutate(twin = b0>0, #1 = non-singleton, 0 = singleton
         v010 = ifelse(v010<100, v010+1900, v010), #year at mother's birth
         b2 = ifelse(b2<100, b2 + 1900, b2),
         mab = b2 - v010, #year at child's birth
         mab_cat = cut(mab, c(min(mab)-1, 19, 24, 29, 34, 39, 44, max(mab))),
         motherid = paste(country_string, motherid),
         mab_cat_num = as.numeric(mab_cat)) %>%
  rename(parity = bord, #bord = birth order
         country = country_string, 
         sex = b4) %>%
  mutate(country = ifelse(country %in% "Maretania", "Mauritania", country),
         country = ifelse(country %in% "Vietnam", "Viet Nam", country)) %>%
  #b2 information is weird!
  filter(! country %in% c("Nepal", "Viet Nam")) %>% 
  filter(! is.na(b2)) %>%
  add_column(e0 = NA,
             u5mr = NA,
             gdp_growth = NA,
             tfr = NA)

# combine -----------------------------------------------------------------

#remove duplicates
dhs1_mothers <- unique(dhs1$motherid)
dhs2_mothers <- unique(dhs2$motherid)
#there shouldn't be overlap between these mothers

dhs2 <- dhs2 %>%
  filter(motherid %in% dhs2_mothers[!(dhs2_mothers %in% dhs1_mothers)])

dhs <- rbind(dhs1, dhs2) 

# add: IMR ------------------------------------------------------------

imrdt <- read_excel("./data/world_bank_infantmortality.xlsx", skip = 1) %>%
  janitor::clean_names() %>%
  pivot_longer(cols = c(5:67),
               names_to = "b2",
               values_to = "imr") %>%
  rename(country = "country_name",
         worldbank_label = "country_code") %>%
  mutate(b2 = substr(b2, start = 2, stop = 5) %>% as.numeric()) %>%
  filter(worldbank_label %in% unique(dhs$worldbank_label))

dhs <- left_join(dhs, 
                  imrdt %>% select("worldbank_label", "b2", "imr"), 
                  by = c("worldbank_label", "b2")) 

# add: HDI ----------------------------------------------------------------

#source: https://github.com/nptodd/BAR/tree/main/data/dev_index/human-development-index_OWID.csv
dev1 <- xlsx::read.xlsx("./data/dev_index.xlsx", sheetIndex = 1, header = TRUE) %>%
  select(2:4) %>%
  rename(worldbank_label = iso3,
         b2 = year) %>%
  filter(!b2 %in% c("MHL", "FSM")) %>%
  mutate(b2 = as.numeric(b2))

#source: https://hdr.undp.org/data-center/documentation-and-downloads
dev2 <- readr::read_csv("./data/HDR21-22_Composite_indices_complete_time_series.csv",
                        col_select = c(1, 6:37),
                        show_col_types = FALSE) %>%
  pivot_longer(cols = starts_with("hdi_"), 
               names_to = "year", 
               names_prefix = "hdi_",
               values_to = "hdi_lt") %>%
  rename(worldbank_label = iso3,
         b2 = year) %>%
  mutate(b2 = as.numeric(b2))

dev <- rbind(dev2,
             dev1[dev1$b2<1990,]) %>% #only take pre-1990 data from dev1
  arrange(worldbank_label, b2)

dhs <- left_join(dhs, dev, by = c("worldbank_label", "b2")) %>%
  rename(hdi = hdi_lt) %>%
  relocate(hdi, .after = b2)

# add: GDP per capita -----------------------------------------------------

gdpdata <- read_excel("./data/world_bank_gdp_per_capita.xls", col_names = FALSE, skip = 3)

names(gdpdata) <- gdpdata[1,]

gdpdata <- gdpdata[-1, -c(3:4)] %>%
  pivot_longer(cols = c(3:64),
               names_to = "b2",
               values_to = "gdp_per_capita") %>%
  janitor::clean_names() %>%
  rename(worldbank_label = "country_code") %>%
  mutate(b2 = as.numeric(b2))

dhs <- left_join(dhs, gdpdata, by = c("worldbank_label","b2"))

# add: Region -------------------------------------------------------------

dhs <- left_join(dhs, 
                 readxl::read_excel("./data/country_region.xlsx") %>%
                   rename(worldbank_label = "alpha-3",
                          subregion = "sub-region") %>%
                   select(worldbank_label, region, subregion), 
                 by = "worldbank_label")

# add: birth year ("b2") grouping -----------------------------------------

uniqcountry <- unique(dhs$country)

# 1. how many observations? -----------------------------------------------

plot(seq(1000, 5000, by = 100),1-pbinom(50, size = seq(1000, 5000, by = 100), prob = 17/1000))

min_obs <- 4000

# 2. create country-specific birth year (b2) bins -------------------------

options(warn=2)

percountry <- dhs %>%
  group_by(country, b2) %>%
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


# 2. For birth year groups with less than min_obs --------

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

# 3. assign the country-specific bins to the dhs data ---------------------

dhs <- left_join(dhs, 
                 percountry %>% select(country, b2, b2group), 
                 by = c("country", "b2"))

saveRDS(dhs, './data/dhs.RDS')
