library(haven)
library(tidyverse)
library(data.table)
library(tibble)

# date variables for Ethiopia, Nepal adjusted based on the following info:
# https://dhsprogram.com/data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm
# exception was Afghanistan (CMC conversion same as other countries, despite different calendars)

# 1. Reading in DHS -------------------------------------------------------
# initially assembled by Dr. Kieron Barclay, "wfs_dhs_ffs_ggs_2020_05.dta"

#1-1. reading in the originally provided stata data
# dhs <- subset(read_dta("data/wfs_dhs_ffs_ggs_2020_05.dta"),
#               data_source == "dhs")
# saveRDS(dhs, './data/dt_dhs.RDS')

#1-2. processing only the dhs
dhs1 <- readRDS('./data/dt_dhs.RDS') %>%
#  select(-c(26:35), -c(37:42), harm_mat_edu, e0, u5mr, gdp, gdp_growth, tfr) %>%
  rename(parity = bord, #bord = birth order
         country = country_string, 
         sex = b4) %>%
  mutate(twin = b0>0, #1 = non-singleton, 0 = singleton
         year_survey = floor((v008 - 1) / 12) + 1900, #v008 = CMC survey year
         #v010 = ifelse(v010<100, v010 + 1900, v010), #v010 = year at mother's birth
         year_mom_birth = floor((v011 - 1) / 12) + 1900, #v011 = CMC mother's birth
         #b2 = ifelse(b2<100, b2 + 1900, b2), #b2 = year at child's birth
         year_child_birth = floor((b3 - 1) / 12) + 1900, #b3 = CMC child's birth
         #mab = b2 - v010, 
         mab = (b3 - v011) / 12,
         motherid = paste(country, year_survey, caseid),
         survey = "DHS") %>%
  select(-c("cc_numeric","setsize","caseid","maternal_education","mother_birthyear","true_birthorder","death_year","death_month"))

# 2. Reading in additional DHS -------------------------------------------------------

dhs2 <- readRDS("./data/dt_dhs_new_1.rds") %>%
  filter(! country_string %in% c("Viet Nam", "Mauretania")) %>% #appears that the Mauritania data is actually Madagascar VII, hence dropping
  filter(! v000 %in% c("GN6", "LB", "LB5", "LB6", "NI2", "NI3", "NI5", "NI6")) %>%
  rename(caseid = motherID,
         parity = bord, #bord = birth order
         country = country_string, 
         sex = b4) %>%
  mutate(twin = b0>0, #1 = non-singleton, 0 = singleton
         year_survey = ifelse(v000 %in% "NP3",
                              floor((v008 + 519 - 1) / 12) + 1900, #v008 = CMC survey year
                              ifelse(v000 %in% c("NP4","NP5","NP6","NP7"),
                                     floor((v008 - 681 - 1) / 12) + 1900,
                                     floor((v008 - 1) / 12) + 1900)
                              ),
         year_survey = ifelse(country %in% "Ethiopia",
                              floor((v008 + 92 - 1) / 12) + 1900,
                              year_survey
                              ),
         #v010 = ifelse(v010<100, v010 + 1900, v010), #v010 = year at mother's birth
         year_mom_birth = ifelse(v000 %in% "NP3",
                                 floor((v011 + 519 - 1) / 12) + 1900, #v011 = CMC mother's birth
                                 ifelse(v000 %in% c("NP4","NP5","NP6","NP7"),
                                        floor((v011 - 681 - 1) / 12) + 1900,
                                        floor((v011 - 1) / 12) + 1900)
                                 ), 
         year_mom_birth = ifelse(country %in% "Ethiopia",
                                 floor((v011 + 92 - 1) / 12) + 1900,
                                 year_mom_birth
                                 ),
         #b2 = ifelse(b2<100, b2 + 1900, b2), #b2 = year at child's birth
         year_child_birth = ifelse(v000 %in% "NP3",
                                   floor((b3 + 519 - 1) / 12) + 1900, #b3 = CMC child's birth
                                   ifelse(v000 %in% c("NP4","NP5","NP6","NP7"),
                                          floor((b3 - 681 - 1) / 12) + 1900,
                                          floor((b3 - 1) / 12) + 1900)
                                   ),
         year_child_birth = ifelse(country %in% "Ethiopia",
                              floor((b3 + 92 - 1) / 12) + 1900,
                              year_child_birth
                              ),
         #mab = b2 - v010, 
         mab = (b3 - v011) / 12,
         motherid = paste(country, year_survey, caseid)) %>% #here, 'motherid' seems to be 'caseid'
  mutate(#country = ifelse(country %in% "Mauretania", "Mauritania", country), 
         survey = "DHS",
         zone = country)

dhs3 <- readRDS("./data/dt_dhs_new_2.rds") %>%
  #created from "U:/cloud/projects/Ongoing/twinage/scripts/0_1_dhs_reading_adding_DHS.R"
  mutate(survey = "DHS",
         zone = country)

# add WFS -----------------------------------------------------------------

wfs <- readRDS('./data/wfs_SSA&SA.rds') %>%
  filter(!country %in% "Mauritania") %>%
  mutate(survey = "WFS", 
         zone = country) 

# combine -----------------------------------------------------------------

#remove duplicates
dhs1_surveys <- unique(dhs1$v000)
dhs2_surveys <- unique(dhs2$v000)
dhs3_surveys <- unique(dhs3$v000)
#there shouldn't be overlap between these mothers

intersect(dhs1_surveys, dhs2_surveys)
intersect(dhs1_surveys, dhs3_surveys)
intersect(dhs2_surveys, dhs3_surveys) #ET4 in dhs3 is for 2005. There are two ET4 (2000, 2005)

vars <- intersect(names(dhs1), names(dhs2))
vars <- intersect(vars, names(dhs3))
vars <- intersect(vars, names(wfs))
 
dhs_combined <- rbind(dhs1 %>% select(all_of(vars)),
                      dhs2 %>% select(all_of(vars)),
                      dhs3 %>% select(all_of(vars)),
                      wfs %>% select(all_of(vars)))

# Parity for multiple births should be the same ---------------------------

# motherid <- dhs_combined$motherid
# uniq_moms <- unique(motherid[dhs_combined$twin])
# birth_year <- dhs_combined$year_child_birth
# parity <- dhs_combined$parity
# 
# options(warn=2)
# 
# for(i in 1:length(uniq_moms)){
#   
#   cur_mother_filter <- motherid %in% uniq_moms[i]
#   cur_births <- dhs_combined[cur_mother_filter, ]
#   
#   first_child_of_all_multiple_births <- which(cur_births$b0 == 1)
#   
#   if(length(first_child_of_all_multiple_births) != 0){
#     
#     for(j in 1:length(first_child_of_all_multiple_births)){
#       
#       cur_child_birth <- cur_births$year_child_birth[first_child_of_all_multiple_births[j]]
#       
#       cur_filter <- cur_mother_filter & (birth_year == cur_child_birth)
#       
#       parity[cur_filter] <- min(parity[cur_filter])
#       
#     } 
#     
#   }
# 
# }
# 
# dhs_combined$parity_new <- parity

saveRDS(dhs_combined, './data/dhs.RDS')
