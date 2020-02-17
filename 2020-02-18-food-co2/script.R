library(tidyverse)
library(ggmap)
library(cartogram)

source("theme.R")

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
countries <- read_csv("~/Desktop/countries.csv")

df_top <- df %>% 
  group_by(country) %>% 
  filter(co2_emmission == max(co2_emmission)) %>% 
  select(country, top_food = food_category) %>% 
  ungroup()

df_sum <- df %>% 
  group_by(country) %>% 
  summarise(co2_emmission = sum(co2_emmission)) %>% 
  ungroup() %>% 
  left_join(df_top) %>% 
  mutate(country = recode(country,
                          Congo = "Congo [DRC]",
                          `Hong Kong SAR. China` = "Hong Kong",
                          Macedonia = "Macedonia [FYROM]",
                          Myanmar = "Myanmar [Burma]",
                          `Taiwan. ROC` = "Taiwan",
                          USA = "United States"))

df_top_countries <- df_sum %>% 
  left_join(countries, by = c("country" = "name"))

# Plot
world_map <- df_top %>%
  mutate(country = recode(country,
                          Congo = "Democratic Republic of the Congo",
                          `Taiwan. ROC` = "Taiwan",
                          `United Kingdom` = "UK")) %>% 
  left_join(map_data("world"), by = c("country" = "region"))

full_world <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#4d4d4d", color = "#666666") +
  geom_point(data = df_top_countries, aes(longitude, latitude, size = co2_emmission, col = top_food),
             alpha = 0.5) +
  scale_size(range = c(1, 13)) +
  guides(size = FALSE,
         col = guide_legend(title = "Food type with highest Co2 emissions",
                            override.aes = list(size = 10, alpha = 1))) +
  labs(title = "Carbon dioxide emissions from the food industry",
       subtitle = "The map shows the most CO2-emitting food type in each country (measured in kg CO2/person/year). Bubbles are scaled\nby the country's overall emission from food. When data is not available, the boundaries for that country are not shown.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
