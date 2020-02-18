library(tidyverse)
library(ggmap)
library(cartogram)

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
countries <- read_csv("~/tidy-tuesday/2020-02-18-foods-carbon-footprint/countries.csv")
population <- read_csv("~/tidy-tuesday/2020-02-18-foods-carbon-footprint/population.csv") %>% 
  filter(Time == 2020) %>% 
  select(Location, PopTotal) %>% 
  distinct() %>% 
  mutate(Location = recode(Location,
                           `Bolivia (Plurinational State of)` = "Bolivia",
                           `Democratic Republic of the Congo` = "Congo [DRC]",
                           Czechia = "Czech Republic",
                           `Iran (Islamic Republic of)` = "Iran",
                           Myanmar = "Myanmar [Burma]",
                           `Russian Federation` = "Russia",
                           `United States of America` = "United States",
                           `Venezuela (Bolivarian Republic of)` = "Venezuela",
                           `Viet Nam` = "Vietnam"))

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
  left_join(countries, by = c("country" = "name")) %>% 
  left_join(population, by = c("country" = "Location")) %>% 
  mutate(total_emissions = co2_emmission * PopTotal)

world_map <- df_top %>%
  mutate(country = recode(country,
                          Congo = "Democratic Republic of the Congo",
                          `Taiwan. ROC` = "Taiwan",
                          `United Kingdom` = "UK")) %>% 
  left_join(map_data("world"), by = c("country" = "region"))

full_world <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#4d4d4d", color = "#666666") +
  geom_point(data = df_top_countries, aes(longitude, latitude, size = total_emissions, col = top_food),
             alpha = 0.5) +
  scale_size(range = c(1, 20)) +
  guides(size = FALSE,
         col = guide_legend(title = "Food type with highest Co2 emissions",
                            override.aes = list(size = 10, alpha = 1))) +
  labs(title = "The carbon footprint of the food industry",
       subtitle = "The map shows the most CO2-emitting food type in each country (measured in kg CO2/person/year). Bubbles are scaled by\nthe country's total emission from food. When data is not available, the boundaries for that country are not shown.") +
  theme(axis.text =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        legend.key = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        text = element_text(color = "white", family = "Raleway"))
