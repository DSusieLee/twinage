library(tidyverse)
library(hrbrthemes)

dt <- readRDS("./results/stan_outputs/230703_age_parity_imr_gdp.rds")
posterior <- as.data.frame(dt)

#which vars are countries?
country_var_filter <- str_detect(names(posterior), "factor")

posterior[, which(country_var_filter)] <- posterior[, which(country_var_filter)] + posterior[, 1]

country_vars <- c(1, which(country_var_filter))

#make country variables simpler
var_names <- names(posterior) %>%
  str_replace_all(., "[()]", "") %>%
  str_remove(., pattern = "factorcountry") 

names(posterior) <- var_names
names(posterior)[1] <- "Afghanistan"

# set predicting values ---------------------------------------------------

mab_min <- 15
mab_max <- 49

mab_pred <- seq(mab_min-12, mab_max-12, by = 1) 
#because mab was scaled such that 0 equals to the smallest mab (12)

# age effects predicted based on posterior  ---------------------------

data <- posterior[, c(2,3)] %>% 
  rename(mab1 = "polymab_scaled, 2, raw = TRUE1",
         mab2 = "polymab_scaled, 2, raw = TRUE2") %>%
  mutate(vertex = -mab1/(2*mab2) + 12)

saveRDS(data, "./results/230320_age_posterior&vertex.rds")
 
predicted <- expand.grid(mab_pred,
                         c(1:10000),
                         NA) %>%
  rename(mab = Var1,
         sim = Var2,
         twin_prob = Var3)

length_each_pred_round <- length(mab_pred)
k <- 1

for(i in 1:10000){
  
  predicted$twin_prob[k:(k + length_each_pred_round - 1)] <-
    data$mab1[i]*mab_pred + data$mab2[i]*(mab_pred^2) 
  #age specific twinning propensity BEFORE baseline twinning rate of a country is added
  
  k <- k + length_each_pred_round
  
}

saveRDS(predicted 
        %>% mutate(mab = mab + 12,
                   mab_cat = cut(mab, c(-1, 19, 24, 29, 34, 39, 44, 60))), 
        file = "./results/230510_age_effects.rds")