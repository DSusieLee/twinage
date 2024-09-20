library(tidyverse)
library(hrbrthemes)
library(Cairo)

countries <- readRDS('./data/projections_age_combined.rds') %>%
                      select(country)

countries <- unique(countries$country)

d <- readRDS('./data/projection_data_ageprop&popsize.rds') %>% 
  filter(Year %in% c(2010, 2050, 2100) &
           source %in% "WPP" &
           country %in% countries) %>%
  mutate(Year = as.factor(Year))

# changes in mab prop -----------------------------------------------------

mab_prop <- readRDS('./data/projection_data_ageprop&popsize.rds') %>%
  #created in "0_3_data_for_projections.R"
  filter(! source %in% "DHS") %>%
  group_by(country, Year) %>%
  summarize(prop_over_25 = sum(mab_prop[mab_cat %in% c("25-29", "30-34", "35-39", "40-44", "45-49")])) %>%
  ungroup()

mab_prop_by_country <- mab_prop %>%
  group_by(country) %>%
  mutate(mab_prop_change = 100*(prop_over_25 - prop_over_25[Year == 2010])/prop_over_25[Year == 2010]) %>%
  filter(Year == 2100) %>%
  arrange(mab_prop_change)

d$country <- factor(as.factor(d$country), 
                    levels = mab_prop_by_country$country)

# all countries -----------------------------------------------------------

g1 <- ggplot(d %>% filter(!country %in% "Maldives"), 
       aes(x = mab_cat, 
           y = mab_prop, 
           group = Year, 
           colour = Year)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(direction = -1, option = "plasma") +
  facet_wrap(vars(country), ncol = 5) +
  labs(
    title = "Proportion of births by maternal age over time",
    subtitle = "World Population Prospects projections",
    y = "Proportion",
    x = "Maternal age at birth (5 yrs category)") +
  theme_ipsum(axis_title_size = 15,
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave('./figures/231208_prop_mab_by_country.png', type = "cairo")

CairoSVG(file.path('./figures' ,"240115_prop_mab_change_overtime_by_country.svg"), width = 9, height = 12, bg="transparent")
g1
dev.off()

# Malawi and Korea (only 2010) --------------------------------------------------------

mab_prop <- readRDS("./data/births_1997_2019_Btype.rds") %>%
  filter(reportY == 2010 & mother_age %in% c(2:8)) %>%
  rename(mab_cat = mother_age) %>%
  group_by(mab_cat) %>%
  summarise(n = n()) %>%
  mutate(mab_prop = n / sum(n), 
         b2 = 2010,
         scenario = "hypothetical",
         mab_cat = unique(d$mab_cat)) 

g <- ggplot(d %>% filter(country %in% "Malawi"), 
       aes(x = mab_cat, 
           y = mab_prop)) +
  geom_line(aes(group = Year,
                colour = Year)) +
  theme_ipsum(axis_title_size = 15,
              base_size = 12,
              plot_margin = margin(10,10,10,10))

g + geom_line(data = mab_prop,
            aes(x = mab_cat, 
                y = mab_prop,
                group = 1,
                colour = "S. Korea 2010"), 
            colour = "grey",
            linewidth = 1, 
            linetype = "dashed") +
  annotate(geom = "text", x = 5, y = 0.4, label = "Korea in 2010") 

