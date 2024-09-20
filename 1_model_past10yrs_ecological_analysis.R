library(tidyverse)
library(haven)
library(lfe)
library(broom)

cur_dt <- readRDS('./data/dhs_past10yrs.RDS') %>% 
  filter(b0 <= 1 & #this makes each row representing one delivery rather than birth
           between(mab, 15, 49),
         (subregion %in%  c("Sub-Saharan Africa", "Southern Asia"))) %>%
  filter(!(country %in% "India" & year_child_birth >= 2000) & 
           (!country %in% "South Africa")) %>%
  mutate(gdp_per_capita = as.numeric(gdp_per_capita),
         mab_cat = cut(mab, c(14, seq(19, 44, by= 5), 49)),
         no_prime_educ = harm_mat_edu == 0,
         only_prime_educ = harm_mat_edu <= 1,
         recall_duration = year_survey - year_child_birth) %>%
  filter(!(country %in% c("Central African Republic", "Gambia", "Comoros", "Maldives")))

# time trends by country --------------------------------------------------

data <- cur_dt %>% 
  group_by(country, year_child_birth_group) %>%
  summarize(
    n = n(),
    year = mean(year_child_birth) %>% round(0),
    year_cat  = cut(year, c(-1, seq(1980, 2015, by = 5), 2040)),
    year_cat2 = cut(year, c(-1, 1980, 1990, 2000, 2010, 2040)),
    twinrate = mean(twin, na.rm=T)*1000,
    mean_recall_years = mean(recall_duration),
    mab_mean = mean(mab), 
    mab_mean_cat = cut(mab_mean, c(-1, 26, 26.5, 27, 60)),
    mab_1b = mean(mab[parity == 1]),
    mab_1b_cat = cut(mab_1b, c(-1, 19.3, 20, 20.8, 60)),
    young_mom_num = sum(mab_cat %in% c("(-1,19]", "(19,24]")),
    old_mom_prop = 1-young_mom_num/n,
    gdp = mean(gdp_per_capita, na.rm=T),
    edu_prop = 1-sum(no_prime_educ)/n,
    edu_prop_cat = cut(edu_prop, c(-1, 0.33, 0.66, 1.1))
  ) %>%
  ungroup() 

data_yearcat <- data %>%
  group_by(country, year_cat) %>%
  summarize(
    twinrate = mean(twinrate, na.rm=T),
    mab_mean = mean(mab_mean),
    mab_mean_cat = cut(mab_mean, c(-1, 26, 26.5, 27, 60)),
#    mab_mean_cat = cut(mab_mean, c(-1, 25.5, 26, 26.5, 27, 60)),
    gdp_mean = mean(gdp),
    edu_prop_mean = mean(edu_prop)
  )

# analysis ----------------------------------------------------------------

m1 <- felm(twinrate ~ mab_mean_cat | country + year_cat, data = data_yearcat)
m2 <- felm(twinrate ~ mab_mean_cat + gdp_mean | country + year_cat, data = data_yearcat)
m3 <- felm(twinrate ~ mab_mean_cat + gdp_mean + edu_prop_mean | country + year_cat, data = data_yearcat)

x <- rbind(tidy(m1) %>% mutate(model = "only mab"), 
           tidy(m2) %>% mutate(model = "added gdp"),
           tidy(m3) %>% mutate(model = "added educ"))

write.xlsx(x, "./results/240708_twinrate_ecological_analysis.xlsx")

#comparing country level difference at a given time
m1 <- lm(twinrate ~ 1  + factor(year_cat) + factor(country), data = data_yearcat)
m2 <- update(m1, . ~ . + mab_mean_cat)
#m3 <- lm(twinrate ~ 1  + mab_mean_cat + factor(year_cat)*factor(country), data = data_yearcat)

stargazer::stargazer(m1,m2,
                     omit = "country",
                     ci = TRUE,
                     out = "./results/240212_models.htm")