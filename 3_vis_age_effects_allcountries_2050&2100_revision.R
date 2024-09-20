library(tidyverse)
library(hrbrthemes)
library(ggdist)
library(viridis)
library(Cairo)

colors <- plasma(3, alpha = 1, begin = 0, end = 1, direction = -1)

projection_data <- readRDS('./results/projection_results_240715.rds')

countries <- unique(projection_data$country)

scenarios <- unique(projection_data$scenario)
#[1] "changed_ageprop"      "changed_agestructure" "changed_asfr"        
#[4] "changed_popsize"      "expected_for_2010"    "expected_for_2050"   
#[7] "expected_for_2100" 

metrics <- names(projection_data)[3:6]
#[1] "twin_deliveries" "num_births" "num_deliveries"  "twin_rates" 

# 1. choose scenario & metric ---------------------------------------------------

#1-1. Base
base_scenario <- scenarios[5]
cur_metric <- metrics[4]

#1-2. Scenario of interest
scenario1 <- scenarios[6]
scenario2 <- scenarios[7]

# 2. Calculate differences ------------------------------------------------

projection_data_subset <- projection_data %>% 
  select(country, scenario, all_of(cur_metric)) %>%
  filter(scenario %in% c(base_scenario, scenario1, scenario2)) %>%
  mutate(pchange = list(NA),
         pchange_mean = NA)

names(projection_data_subset)[3] <- "metric"

country <- projection_data_subset$country
base_scenario_filter <- projection_data_subset$scenario %in% base_scenario
metric <- projection_data_subset$metric

for(i in 1:nrow(projection_data_subset)){
  
  cur_country <- country[i]
  base <- metric[country %in% cur_country & base_scenario_filter]
  pchange <- (unlist(metric[i]) - unlist(base))/unlist(base)
  projection_data_subset$pchange[i] <- list(pchange)
  projection_data_subset$pchange_mean[i] <- mean(pchange)
    
}

#arrange countries
country_arrange <- projection_data_subset %>%
  filter(scenario %in% scenario1) %>%
  arrange(pchange_mean)

projection_data_subset$country1 <- factor(projection_data_subset$country,
                                          levels = country_arrange$country)

# 3. Make a figure ------------------------------------------------------------------

cur_label <- c("2050", "2100")

cur_dt <- projection_data_subset %>%
  filter(scenario %in% c(scenario1, scenario2)) %>%
  unnest(pchange) %>%
  mutate(pchange = ifelse(country %in% c("Mozambique","Togo") & scenario %in% scenario1, 
         rep(0.00001, 10000), 
         pchange))

g <- ggplot(cur_dt, aes(x = country1, y = pchange, colour = scenario)) + 
  geom_boxplot(alpha = 0.2) + 
  scale_color_manual(name = "Projected year",
                     values = c(colors[2], colors[3]),
                     labels = cur_label) + 
  scale_y_log10(breaks = c(0.01, 0.1, 1),
                labels = c(1, 10, 100)) +
  labs(x = "",
       y = "% change twinning rate (log scale)",
       caption = ) +
  annotate("text",
           label = '(-) change', 
           x = 1.25, 
           y = 0.00005,
           size = 3,
           angle = 90) +
  theme_ipsum(plot_margin = margin(0,0,0,0, "pt")) + 
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust=1)) 

ggsave('./figures/240130_2050vs2100.png', type = "cairo")
 
CairoSVG(file.path('./figures',"240715_2050vs2100.svg"),  width = 9, height = 6, bg="transparent")
g
dev.off()