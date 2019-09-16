library(tidyverse)
library(gnis)

source("theme.R")

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

park_visits_agg <- park_visits %>% 
  filter(year != "Total") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarise(visitors = sum(visitors))

state_pop_agg <- state_pop %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop))

full <- park_visits_agg %>% 
  left_join(state_pop_agg) %>% 
  left_join(gas_price) %>% 
  select(-gas_current)

full <- full[complete.cases(full),]

main_plot <- full %>% 
  mutate(pop = pop / 1000000) %>% 
  mutate(visitors = visitors / 1000000) %>% 
  ggplot() + 
  geom_line(aes(year, pop), lwd = 1, col = brewer.pal(3, "Set2")[1]) +
  geom_line(aes(year, visitors), lwd = 1, col = brewer.pal(3, "Set2")[2]) +
  annotate("text", label = "Population", x = 1960, y = 200, col = brewer.pal(3, "Set2")[1],
           family = "Raleway", fontface = "bold", size = 6) +
  annotate("text", label = "Visitors", x = 1960, y = 100, col = brewer.pal(3, "Set2")[2],
           family = "Raleway", fontface = "bold", size = 6) +
  labs(title = "Visits to US national parks have stalled – despite a rising population",
       subtitle = "In the 1980s and 1990s, national park visitors outnumbered the US population. However, increasing petrol prices\nhave deterred home travellers, as the number of visitors has levelled off.",
       x = NULL,
       y = NULL,
       caption = "Graphic: Joshua Feldman") +
  scale_y_continuous(labels = scales::unit_format(unit = "m"))

main_plot

sub_plot <- full %>% 
  ggplot(aes(year, gas_constant)) +
  labs(title = "Price of petrol",
       subtitle = "Adjusted for purchasing power",
       x = NULL,
       y = NULL) +
  geom_line(lwd = 1, col = brewer.pal(3, "Set2")[3]) +
  theme(plot.background = element_rect(color = "#666666", fill = "#666666"),
        panel.background = element_rect(color = "#666666", fill = "#666666"),
        panel.grid.major = element_line(color = "#808080", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#808080", size = rel(0.25)),
        text = element_text(size = 12),
        title = element_text(size = 14))

sub_plot <- ggplotGrob(sub_plot)

main_plot +
  annotation_custom(
    grob = sub_plot,
    xmin = 1990,
    xmax = 2015,
    ymin = 25,
    ymax = 150
  )
