library(ggmap)
library(maptools)
library(maps)
library(zoo)
library(tidyverse)
library(gganimate)
library(emoGG)

df <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") %>% 
  mutate(year = as.numeric(str_sub(as.Date(date_time, '%m/%d/%Y'), 1, 4)))

world_map <- borders("world", colour = "white", fill = "white")

animation <- ggplot(data = df) +
  world_map +
  geom_point(aes(x = longitude, y = latitude),
             color = "#6CC417", fill = "#6CC417", shape = 21, alpha = 0.1) +
  labs(title = "Global UFO sightings (Year: {round(frame_time)})",
       subtitle = "Data from the National UFO Reporting Center (NUFORC)",
       caption = "Source: #TidyTuesday\nGraphic: @JoshuaFeIdman") +
  theme_dark(base_size = 16) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "#1a1a1a"),
        panel.background = element_rect(fill = "#1a1a1a"),
        panel.grid = element_blank(),
        text = element_text(family = "Lato", color = "white")) +
  transition_time(year) +
  ease_aes('linear')

animate(animation, height = 800, width = 1200)
