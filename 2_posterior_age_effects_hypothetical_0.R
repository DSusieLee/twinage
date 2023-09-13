library(tidyverse)
library(haven)


cur_dt <- readRDS('./data/dhs.RDS') %>% 
  select(mab, mab_cat, b0, subregion, country, b2) %>%
  filter(  between(mab, 15, 49) &
             b0 <= 1 & #this makes each row representing one delivery rather than birth
             (subregion %in%  c("Sub-Saharan Africa", "Southern Asia")) &
             (!country %in% "South Africa"))  

country_list_data <- unique(cur_dt$country)

mab_category <- levels(cur_dt$mab_cat)

d <- data.frame(mab_cat = rep(mab_category, times = length(country_list_data)),
                country = rep(country_list_data, each = length(mab_category)))

# hypothetical - year 2010 in Korea ---------------------------------------

kor_mab_prop <- readRDS("./data/births_1997_2019_Btype.rds") %>%
  filter(reportY == 2010 & mother_age %in% c(2:8)) %>%
  rename(mab_cat = mother_age) %>%
  group_by(mab_cat) %>%
  summarise(n = n()) %>%
  mutate(prop_2010_korea = n / sum(n)) %>%
  select(-n) %>%
  ungroup()

d$prop_2010_korea <- rep(as.numeric(kor_mab_prop$prop_2010_korea), 
                         times = length(unique(d$country)))


# hypothetical - years 2010 and 2040 according to UNPP ------------------------------------------------

country_list <- c(tolower(unique(country_list_data)),
                  "côte d'ivoire",
                  "congo")

dt_births1 <-  
  readxl::read_excel("./data/WPP2022_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                     sheet = "Estimates", #observed values for previous years
                     skip = 16) %>%
  select(-"Index") %>%
  mutate(across(contains("-"), as.integer)) %>%
  filter((tolower(`Region, subregion, country or area *`) %in% country_list) & Year == 2010) %>%
  select(-c(11,19)) %>%
  rowwise() %>%
  mutate(n = sum(across(c(11:17))),
         across(c(11:17), ~ ./n)) %>%
  ungroup() %>%
  pivot_longer(cols = c(11:17), names_to = "mab_cat") %>%
  rename(country = "Region, subregion, country or area *",
         prop_2010 = value,
         size_2010 = n) %>%
  mutate(
    size_2010 = size_2010*1000,
    country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
    country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country))

dt_births2 <-
  readxl::read_excel("./data/WPP2022_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx", 
                    sheet = "Medium variant", #projected values
                    skip = 16) %>% 
  select(-"Index") %>%
  mutate(across(contains("-"), as.integer)) %>%
  filter(tolower(`Region, subregion, country or area *`) %in% country_list & Year == 2040) %>%
  select(-c(11,19)) %>%
  rowwise() %>%
  mutate(n = sum(across(c(11:17))),
         across(c(11:17), ~ ./n)) %>%
  ungroup() %>%
  pivot_longer(cols = c(11:17), names_to = "mab_cat") %>%
  rename(country = "Region, subregion, country or area *",
         prop_2040 = value,
         size_2040 = n) %>%
  mutate(
    size_2040 = size_2040*1000,
    country = ifelse(country %in% "Congo", "Congo Brazzaville", country),
    country = ifelse(country %in% "Côte d'Ivoire", "Cote D'Ivoire", country))

d$mab_cat <- rep(unique(dt_births1$mab_cat), 
                 times = length(country_list_data))

d <- left_join(d, 
               dt_births1 %>% select(size_2010, mab_cat, prop_2010, country), 
               by = c("country","mab_cat"))
d <- left_join(d, 
               dt_births2 %>% select(size_2040, mab_cat, prop_2040, country), 
               by = c("country","mab_cat"))


# age structure in 2010 available countries from DHS ----------------------

d1 <- cur_dt %>%
  filter(b2 == 2010) %>%
  group_by(country, mab_cat) %>%
  summarise(n = n()) %>%
  mutate(prop_2010_dhs = n / sum(n)) %>%
  select(-n) %>%
  ungroup()

d1$mab_cat <- rep(unique(dt_births1$mab_cat), times = length(unique(d1$country)))

country_df <- as.data.frame(table(d1$country))

d <- left_join(d,
               d1 %>%
                 filter(country %in% country_df$Var1[country_df$Freq == 7]),
               by = c("country", "mab_cat"))

saveRDS(d, "./data/ageprop&popsize.rds")
