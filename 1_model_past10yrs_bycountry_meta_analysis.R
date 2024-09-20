library(tidyverse)
library(haven)
library(spaMM)
library(ROI.plugin.glpk)
library(fixest)
library(broom)
library(tidycat)
library(metafor)
library(hrbrthemes)

cur_dt <- readRDS('./data/dhs_past10yrs.RDS') %>% 
  select(twin, mab, harm_mat_edu, parity, imr, hdi, b0, year_survey, year_child_birth, year_child_birth_group, sex, subregion, country, gdp_per_capita, motherid) %>%
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
         no_prime_educ = harm_mat_edu == 0,
         only_prime_educ = harm_mat_edu <= 1,
         recall_duration = year_survey - year_child_birth)

countries <- readRDS('./data/projections_age_combined.rds') %>%
                    select(country)

cur_dt <- cur_dt %>% 
  filter(country %in% unique(countries$country))

countries <- unique(cur_dt$country)
countries_n <- length(countries)

# model fitting: separately by country -----------------------------------------------------------

for(i in 1:countries_n){
  
  cur_country = countries[i]
  dt_for_fitting <- cur_dt %>% filter(country %in% cur_country)
  
  fit1 <- lm(twin ~ mab_cat + parity_scaled_cat + factor(year_child_birth_group), 
             data = dt_for_fitting)
  
  fit2 <- feols(twin ~ mab_cat | motherid, 
                data = dt_for_fitting)
  
  fit3 <- feols(twin ~ mab_cat + parity_scaled_cat + factor(year_child_birth_group) | motherid, 
                data = dt_for_fitting)
  # 
  # fit3 <- fitme(twin ~ mab_cat + parity_scaled_cat + factor(year_child_birth_group) + (1|motherid), 
  #               data = dt_for_fitting,
  #               family = stats::binomial(link = "logit"),
  #               method = "PQL/L")
  
    if(i == 1){

      outputs <- rbind(
        tidy(fit1) %>%
          mutate(country = cur_country,
                 model = "LPM without mother fixed effects"),
        tidy(fit2) %>%
          mutate(country = cur_country,
                 model = "LPM with mother fixed effects"),
        tidy(fit3) %>%
          mutate(country = cur_country,
                 model = "LPM with mother fixed effects and covariates"))

    }

    else{

      outputs <- rbind(
        outputs,
        tidy(fit1) %>%
          mutate(country = cur_country,
                 model = "LPM without mother fixed effects"),
        tidy(fit2) %>%
          mutate(country = cur_country,
                 model = "LPM with mother fixed effects"),
        tidy(fit3) %>%
          mutate(country = cur_country,
                 model = "LPM with mother fixed effects and covariates")
      )
    }
  }

# meta analysis -----------------------------------------------------------

mab_cat <- as.character(unique(cur_dt$mab_cat))
mab_cat <- mab_cat[c(1,2,3,5,6,7)]
model_types <- unique(outputs$model)

row_num <- length(mab_cat) * length(model_types)

outputs_meta <- data.frame(b = rep(NA, row_num),
                           ci_lb = rep(NA, row_num),
                           ci_ub = rep(NA, row_num),
                           model = rep(NA, row_num),
                           mab_cat = rep(NA, row_num))

k <- 1

for(i in 1:length(mab_cat)){
  
  for(j in 1:length(model_types)){
   
    data <- outputs %>% 
      filter(model %in% model_types[j],
             term %in% paste("mab_cat", mab_cat[i], sep="")) 

    res <- rma.uni(yi = estimate,  
                   sei = std.error,
                   data = data)
    
    if(j == 1){
      forest(res, slab = data$country)}
    
    outputs_meta$b[k] <- as.numeric(res$b)
    outputs_meta$ci_lb[k] <- as.numeric(res$ci.lb)
    outputs_meta$ci_ub[k] <- as.numeric(res$ci.ub)
    outputs_meta$model[k] <- model_types[j]
    outputs_meta$mab_cat[k] <- mab_cat[i]
    
    k <- k+1
    
  }
  
}

ggsave('./figures/240212_meta_mab1.png', type = "cairo")

# unused: visualize different meta analysis models ------------------------

ggplot(outputs_meta,
       aes(x = mab_cat,
           y = b,
           group = model,
           color = model)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = mab_cat, y = b, ymin = ci_lb, ymax = ci_ub),
                position = position_dodge(width = 0.9)) +
  theme_ipsum()
