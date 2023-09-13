library(tidyverse)
library(rstanarm)

cur_dt <- readRDS('./data/dhs.RDS') %>% 
  select(twin, mab, mab_cat, harm_mat_edu, parity, imr, v010, b0, b2, b2group, sex, subregion, country, gdp_per_capita, motherid) %>%
  filter(!is.na(mab_cat) & 
           mab >= 12 &
           !is.na(imr) &
           !is.na(gdp_per_capita) &
           b0 <= 1 & #this makes each row representing one delivery rather than birth
           (subregion %in%  c("Sub-Saharan Africa", "Southern Asia"))) %>%
  mutate(gdp_per_capita = as.numeric(gdp_per_capita),
         mab_scaled = mab - 12, #scale to the minimum maternal age at birth
         imr_scaled = imr - 7.7, #scale to the minimum IMR
         no_prime_educ = harm_mat_edu == 0,
         only_prime_educ = harm_mat_edu <= 1)

cur_dt <- cur_dt %>%
  filter(!(country %in% "India" & b2 >= 2000) & (!country %in% "South Africa"))

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
  str_remove(., pattern = "factorcountry") 

names(posterior) <- var_names
names(posterior)[1] <- "Afghanistan"

posterior[,c(2:(ncol(posterior)-1))] <- posterior[,1] + posterior[,c(2:(ncol(posterior)-1))]

saveRDS(posterior, "./results/stan_outputs/stan_country_only.rds")

# 2. adding covariates ----------------------------------------------------

post0 <- stan_glm(twin ~
                    poly(mab_scaled, 2, raw = TRUE) + 
                    parity +
                    imr_scaled + 
                    log(gdp_per_capita) + 
                    factor(country),
                  prior_intercept = normal(location = 0.01, scale = 0.004, autoscale = FALSE),
                  data = cur_dt,
                  seed = 333,
                  iter = 5000,
                  chains = 4)

saveRDS(post0, "./results/stan_outputs/230703_age_parity_imr_gdp.rds")

post1 <- stan_lm(twin ~ 
                   poly(mab_scaled, 2, raw = TRUE) +
                   parity +
                   imr_scaled*no_prime_educ +
                   factor(country),
                 prior = R2(location = 0.2),
                 data = cur_dt,
                 seed = 333,
                 iter = 5000,
                 chains = 4
)

saveRDS(post1, "./results/stan_outputs/stanlm_imr_educ_interaction.rds")