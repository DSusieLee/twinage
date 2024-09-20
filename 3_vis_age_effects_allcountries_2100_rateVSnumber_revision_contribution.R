library(tidyverse)
library(hrbrthemes)
library(ggdist)
library(viridis)
library(Cairo)
library(patchwork)

projection_data <- readRDS('./results/projection_results_240715.rds')

countries <- unique(projection_data$country)

scenarios <- unique(projection_data$scenario)
#[1] "changed_ageprop"      "changed_agestructure" "changed_asfr"        
#[4] "changed_popsize"      "expected_for_2010"    "expected_for_2050"   
#[7] "expected_for_2100" 

metrics <- names(projection_data)[3:6]
#[1] "twin_deliveries" "num_births" "num_deliveries"  "twin_rates" 

# impact of maternal aging on twin rates ----------------------------------

dt <- projection_data %>%
  select(country, scenario, twin_rates) %>% 
  filter(scenario %in% c(scenarios[2], scenarios[3], scenarios[5], scenarios[7]))

metric <- dt$twin_rates
base_scenario_filter <- dt$scenario %in% scenarios[5]

cur_d <- data.frame(country = countries,
                    mean = NA)
cur_d$diff_vals <- list(NA)
cur_d$diff_pchange <- list(NA)

for(i in 1:length(countries)){
  
  country_filter <- dt$country %in% countries[i]
  
  base <- metric[base_scenario_filter & country_filter]
  scenario1_vals <- metric[dt$scenario %in% scenarios[2] & country_filter]
  scenario2_vals <- metric[dt$scenario %in% scenarios[3] & country_filter]
  
  #due to population aging
  pchange1 <- (unlist(scenario1_vals) - unlist(base))/unlist(base)
  #due to population size
  pchange2 <- (unlist(scenario2_vals) - unlist(base))/unlist(base)
  
  diff_vals <- unlist(scenario2_vals) - unlist(scenario1_vals)
  diff_pchange <- pchange2 - pchange1
  
  cur_d$pchange1[i] <- mean(pchange1)
  cur_d$pchange2[i] <- mean(pchange2)
  cur_d$diff_vals[i] <- list(diff_vals)
  cur_d$diff_pchange[i] <- list(diff_pchange)
  cur_d$mean[i] = mean(diff_pchange)
  
}

#arrange countries
country_arrange <- cur_d %>% 
  arrange(mean)

cur_d$country1 <- factor(cur_d$country,
                         levels = country_arrange$country)

cur_d1 <- cur_d %>%
  unnest(diff_pchange)

cur_d1_1 <- cur_d %>%
  unnest(diff_vals)

g1 <- ggplot(cur_d1, aes(x = country1, y = diff_pchange)) + 
  geom_boxplot(alpha = 0.2) +
 # scale_y_log10() +
  labs(x = "",
       y = "Percent point difference") +
  theme_ipsum(plot_margin = margin(0,0,0,0, "pt")) + 
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust=1)) 

g1_1 <- ggplot(cur_d1_1, aes(x = country1, y = diff_vals)) + 
  geom_boxplot(alpha = 0.2) +
  # scale_y_log10() +
  labs(x = "",
       y = "Percent point difference") +
  theme_ipsum(plot_margin = margin(0,0,0,0, "pt")) + 
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust=1)) 

# impact of maternal aging on twin number ---------------------------------

dt <- projection_data %>%
  select(country, scenario, twin_deliveries) %>% 
  filter(scenario %in% c(scenarios[1], scenarios[4], scenarios[5], scenarios[7]))

metric <- dt$twin_deliveries
base_scenario_filter <- dt$scenario %in% scenarios[5]

cur_d <- data.frame(country = countries,
                    mean = NA)
cur_d$diff_vals <- list(NA)
cur_d$diff_pchange <- list(NA)

for(i in 1:length(countries)){
  
  country_filter <- dt$country %in% countries[i]
  
  base <- metric[base_scenario_filter & country_filter]
  scenario1_vals <- metric[dt$scenario %in% scenarios[1] & country_filter]
  scenario2_vals <- metric[dt$scenario %in% scenarios[4] & country_filter]
  
  #due to population aging
  pchange1 <- (unlist(scenario1_vals) - unlist(base))/unlist(base)
  #due to population size
  pchange2 <- (unlist(scenario2_vals) - unlist(base))/unlist(base)
  
  diff_vals <- unlist(scenario2_vals) - unlist(scenario1_vals)
  diff_pchange <- pchange2 - pchange1
  
  cur_d$pchange1[i] <- mean(pchange1)
  cur_d$pchange2[i] <- mean(pchange2)
  cur_d$diff_vals[i] <- list(diff_vals)
  cur_d$diff_pchange[i] <- list(diff_pchange)
  cur_d$mean[i] = mean(diff_pchange)
  
}

#arrange countries
country_arrange <- cur_d %>% 
  arrange(mean)

cur_d$country1 <- factor(cur_d$country,
                         levels = country_arrange$country)

cur_d <- cur_d %>%
  unnest(diff_pchange)

g2 <- ggplot(cur_d, aes(x = country1, y = diff_pchange)) + 
  geom_boxplot(alpha = 0.2) +
  # scale_y_log10() +
  labs(x = "",
       y = "Percent point difference") +
  theme_ipsum(plot_margin = margin(0,0,0,0, "pt")) + 
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust=1))


# combine figures and save ------------------------------------------------

CairoSVG(file.path('./figures',"240715_contributions.svg"),  width = 15, height = 7, bg="transparent")
g1+g2
dev.off()

CairoSVG(file.path('./figures',"240715_contributions_vertical.svg"),  width = 10, height = 14, bg="transparent")
g1/g2
dev.off()
