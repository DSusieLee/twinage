library(tidyverse)
library(haven)
library(rstanarm)
library(spaMM)
library(ROI.plugin.glpk)

cur_dt <- readRDS('./data/dhs_past10yrs.RDS') %>% 
  filter(b0 <= 1 & #this makes each row representing one delivery rather than birth
           between(mab, 15, 49),
         (subregion %in%  c("Sub-Saharan Africa", "Southern Asia"))) %>%
  filter(!(country %in% "India" & year_child_birth >= 2000) & 
           (!country %in% "South Africa")) %>%
  mutate(gdp_per_capita = as.numeric(gdp_per_capita),
         mab_scaled = mab - 12, #scale to the minimum maternal age at birth
         mab_cat = cut(mab, c(14, seq(19, 44, by= 5), 49)),
         parity_scaled = parity - 1, #scale to parity 1, so that maternal age estimates reflect that of parity 1
         parity_scaled_cat = ifelse(parity_scaled>=5, "4", as.character(parity_scaled)),
         year_cat = cut(year_child_birth, c(-1, 1980, 1990, 2000, 2010, 2040)),
         no_prime_educ = harm_mat_edu == 0,
         only_prime_educ = harm_mat_edu <= 1,
         recall_duration = year_survey - year_child_birth)

# country fixed effects using LPM -----------------------------------------

# 1. country fixed effects ------------------------------------------------

post_country <- stan_glm(twin ~
                           factor(country),
                         prior_intercept = normal(location = 0.01, scale = 0.004, autoscale = FALSE),
                         prior = normal(location = 0.007, scale = 0.004, autoscale = FALSE),
                         data = cur_dt,
                         seed = 333,
                         iter = 5000,
                         chains = 4)

posterior <- as.data.frame(post_country)
var_names <- names(posterior) %>%
  str_replace_all(., "[()]", "") %>%
  str_remove(., pattern = "factor(country)") 

names(posterior) <- var_names
names(posterior)[1] <- "Afghanistan"

posterior[,c(2:(ncol(posterior)-1))] <- posterior[,1] + posterior[,c(2:(ncol(posterior)-1))]

saveRDS(posterior, "./results/stan_outputs/stan_country_only.rds")

#posterior distribution
data <- posterior %>%
  pivot_longer(cols = everything(), names_to = "country")

country_average <- data %>%
  group_by(country) %>% 
  summarize(mean_twin_rate = mean(value)) %>%
  arrange(mean_twin_rate)

data$country1 <- factor(as.factor(data$country), levels = country_average$country)

saveRDS(data %>% select(-country) %>% rename(country = country1), 
        file = './results/stan_outputs/country_twinrates.RDS')

# 2. adding covariates ----------------------------------------------------

post0 <- stan_glm(twin ~
                    mab_cat*parity_scaled_cat +
                    factor(country),
                  prior_intercept = normal(location = 0.01, scale = 0.004, autoscale = FALSE),
                  data = cur_dt,
                  seed = 333,
                  iter = 5000,
                  chains = 4)

saveRDS(post0, "./results/stan_outputs/stanlm_mab_cat.rds")
saveRDS(post0, "./results/stan_outputs/stanlm_mab_cat_only.rds")

post1 <- update(post0, . ~ . + year_cat)

post1 <- stan_glm(twin ~
                    poly(mab_scaled, 2, raw = TRUE) + 
                    factor(parity_scaled) + 
                    log(gdp_per_capita) + 
                    recall_duration +
                    factor(country),
                  prior_intercept = normal(location = 0.01, scale = 0.004, autoscale = FALSE),
                  data = cur_dt,
                  seed = 333,
                  iter = 5000,
                  chains = 4)

plot(post1, "trace", pars = c("poly(mab_scaled, 2, raw = TRUE)1", "poly(mab_scaled, 2, raw = TRUE)2", "parity", "imr_scaled", "log(gdp_per_capita)"))
plot(post1, "dens_overlay", pars = c("poly(mab_scaled, 2, raw = TRUE)1", "poly(mab_scaled, 2, raw = TRUE)2", "parity", "imr_scaled", "log(gdp_per_capita)"))

post2 <- stan_glm(twin ~
                    poly(mab_scaled, 2, raw = TRUE) + 
                    parity
                    log(gdp_per_capita) + 
                    only_prime_educ*imr_scaled +
                    factor(country),
                  prior_intercept = normal(location = 0.01, scale = 0.004, autoscale = FALSE),
                  data = cur_dt,
                  seed = 333,
                  iter = 5000,
                  chains = 4)
