library(tidyverse)
library(fixest)
library(rstanarm)

cur_dt <- readRDS('./data/dhs.RDS') %>% 
  select(twin, mab, mab_cat, harm_mat_edu, parity, imr, v010, b0, b2, sex, subregion, country, gdp_per_capita, motherid) %>%
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

# results presented in Table 1 --------------------------------------------

fixest1 <- feols(fml = twin ~
                   1 | country, 
                 data = cur_dt)

fixest2 <- feols(fml = twin ~
                   mab_scaled + 
                   I(mab_scaled^2) | country, 
                 data = cur_dt)

fixest3 <- update(fixest2, . ~ . + parity)
fixest4 <- update(fixest3, . ~ . + imr_scaled)
fixest5 <- update(fixest4, . ~ . + log(gdp_per_capita))
fixest6 <- update(fixest3, . ~ . + log(gdp_per_capita))
fixest7 <- update(fixest4, . ~ . + no_prime_educ)

fixest8 <- feols(fml = twin ~
                   mab_scaled*imr_scaled + 
                   I(mab_scaled^2) | country, 
                 data = cur_dt)

fixest9 <- update(fixest4, . ~ . + imr_scaled*no_prime_educ)

# posteriors using stan ---------------------------------------------------
# 1. country fixed effects only -------------------------------------------

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
