library(tidyverse)
library(readxl)
library(ggdist)
library(hrbrthemes)
library(cowplot)

cur_year <- 2020

posterior <- readRDS("./results/stan_outputs/230703_age_parity_imr_gdp.rds") %>% as.data.frame()

# reading in twinning rate ------------------------------------------------

twindt <- readRDS('./results/stan_outputs/country_twinrates.RDS') %>%
  rename(twin_rate = value) 

# reading in # of births --------------------------------------------------

dt <- read_excel("data/WPP2022_FERT_F03_BIRTHS_BY_SINGLE_AGE_OF_MOTHER.xlsx", 
                 #sheet = "Medium variant", #projected values
                 sheet = "Estimates", #observed values for previous years
                 skip = 16) %>%
  select(-"Index") %>%
  mutate(across(as.character(15:49), as.integer))

dt_births <- dt %>% 
  filter(Year %in% cur_year) %>% 
  rowwise() %>%
  mutate(n = sum(across(as.character(15:49)))*1000) %>%
  rename(country = "Region, subregion, country or area *")

# subset to countries present in both dt_births and twindt ----------------

uniq_country <- intersect(dt_births$country, twindt$country)

twindt <- twindt %>% filter(country %in% uniq_country) %>% arrange(country)
dt_births <- dt_births %>% filter(country %in% uniq_country) %>% arrange(country)

# calculate number of twin deliveries in a given year ---------------------------------

twin_rate <- twindt$twin_rate
num_births <- dt_births$n

uniq_country <- unique(twindt$country)

num_twin_deliveries <- rep(NA, nrow(twindt))
num_total_deliveries <- rep(NA, nrow(twindt))
num_total_births <- rep(NA, nrow(twindt))

for(i in c(1:length(uniq_country))){ #number of unique countries
  
  cur_country <- uniq_country[i]
  cur_filt <- twindt$country %in% cur_country
  
  num_twin_deliveries[cur_filt] <- 
    num_births[i]*twin_rate[cur_filt]/(1 + twin_rate[cur_filt])
  
  num_total_deliveries[cur_filt] <- 
    num_births[i] - num_births[i]*twin_rate[cur_filt]/(1 + twin_rate[cur_filt])
  
  num_total_births[cur_filt] <- num_births[i]
  
}

twindt$num_twin_deliveries <- num_twin_deliveries
twindt$num_total_deliveries <- num_total_deliveries
twindt$num_total_births <- num_total_births

# If SDG is achieved (IMR = 18.5) or IMR drops to Hong Kong level (1.17) -----------------------------------------

#bring in IMR data for cur_year = 2020
IMR_dt <- read_csv("data/UN IGME 2022.csv") %>%
  janitor::clean_names() %>%
  filter(indicator %in% "Infant mortality rate" &
           geographic_area %in% uniq_country &
           sex %in% "Total" &
           series_name %in% "UN IGME estimate") %>%
  mutate(time_period = substr(time_period, 1, 4) %>% as.numeric()) %>%
  filter(time_period == cur_year)

#add IMR information for year 2020 to twindt
twindt_updated <- left_join(twindt, 
                    IMR_dt %>% 
                      rename(country = geographic_area) %>% 
                      select(country, obs_value), 
                    by = "country") %>%
  rename(imr = obs_value)

#add to the twindt 
#1) IMR difference between 2020 and SDG target 18.5
#2) IMR effect estimated by the stan model
#3) additional twin deliveries due to the IMR drop to SDG target

twindt_updated <- twindt_updated %>%
  arrange(country) %>%
  mutate(
    imr_2020_to_sdg = imr - 18.5,
    imr_2020_to_hongkong = imr - 1.17,
    imr_effect = rep(posterior$imr_scaled*(-1), length(uniq_country)),
    num_additional_twin_deliveries_sdg = num_total_deliveries*imr_effect*imr_2020_to_sdg,
    num_additional_twin_deliveries_hongkong = num_total_deliveries*imr_effect*imr_2020_to_hongkong
    )

# visualization: absolute number of excess ---------------------------------------------------

# twindt_df <- twindt_updated %>% 
#   filter(imr_2020_to_sdg > 0) %>% #exclude those already achieved SDG
#   group_by(country) %>%
#   median_qi(num_additional_twin_deliveries_sdg, .width = c(.50, .80, .95)) 

p <- 
  ggplot(data = twindt_updated %>% 
         filter(num_additional_twin_deliveries_hongkong > 0) %>% #exclude those already achieved the milestone in IMR
         mutate(country_by_births = fct_reorder(country, num_total_births, .fun = "mean")),
       aes(y = num_additional_twin_deliveries_hongkong, 
           #y = num_additional_twin_deliveries_sdg,
           x = country_by_births)) +
  geom_boxplot() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  annotation_logticks(sides = 'lr') +
  labs(x = "",
       y = "Excess twin deliveries",
       title = "How many more twins would be born if IMR drops to 1.17?",
       subtitle = "1.17 is IMR of Hong Kong in 2020") +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10)) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggdraw(add_sub(p, 
               "(Countries ordered by smallest to largest number women giving births in 2020.)", 
               y = 8,
               x= 1,
               hjust = 1))

ggsave('./figures/230602_hypothetical_IMR_hongkong.png', type = "cairo")

# visualization: percent point increase in children who are twins ---------

scenarios <- c("num_additional_twin_deliveries_sdg", 
               "num_additional_twin_deliveries_hongkong")

pchange =
  100*(((twindt_updated[, scenarios[2]] + twindt$num_twin_deliveries)/twindt$num_twin_deliveries)-1)

twindt_updated$pchange <- pchange$num_additional_twin_deliveries_hongkong

twindt_updated_bycountry <- twindt_updated %>%
  group_by(country) %>%
  summarize(mean = gmodels::ci(pchange)[1],
            lowCI = gmodels::ci(pchange)[2],
            hiCI = gmodels::ci(pchange)[3]) %>%
  arrange(mean)

ggplot(data = twindt_updated %>%
         filter(percent_increase_twin_deliveries >= 1) %>%
         mutate(country_by_births = fct_reorder(country, num_total_births, .fun = "mean"),
                country_by_twins = fct_reorder(country, twin_rate, .fun = "mean"),
                country_by_imr = fct_reorder(country, imr, .fun = "mean")), 
       aes(y = 100*(percent_increase_twin_deliveries - 1), 
           x = country_by_imr)) +
  geom_boxplot() +
  labs(x = "",
       y = "% increase in the number of twin deliveries") +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

