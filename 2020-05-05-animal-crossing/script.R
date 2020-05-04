library(tidyverse)

th <- theme_dark(base_size = 18) +
  theme(axis.text = element_text(color = "white", family = "Raleway"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold"),
        legend.background = element_rect(fill = "#262626", color = "#262626"),
        legend.key = element_rect(fill = "#262626", color = "#262626"),
        panel.background = element_rect(fill = "#262626", color = "#262626"),
        panel.grid.major = element_line(color = "#404040", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#404040", size = rel(0.25)),
        plot.background = element_rect(fill = "#262626", color = "#262626"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 14),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#1a1a1a"),
        text = element_text(color = "white", family = "Raleway"))

theme_set(th)

villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

villagers <- villagers %>% 
  mutate(gender = tools::toTitleCase(gender)) %>% 
  mutate(species = tools::toTitleCase(species)) %>% 
  mutate(personality = tools::toTitleCase(personality))

villagers_species_personality <- villagers %>% 
  group_by(gender, species) %>% 
  count(personality) %>% 
  ungroup()

villagers_species_personality %>% 
  mutate(species = fct_rev(species)) %>% 
  mutate(gender = fct_rev(gender)) %>% 
  ggplot(aes(personality, species, size = n, color = personality)) +
  geom_point() +
  labs(title = "Species and personality type in AC: New Horizons",
       subtitle = "There are 391 villagers in the game, comprising 35 species and 8 personality types.\nThe most common combination is a Peppy Rabbit, followed by a Lazy Dog.",
       x = "Personality type",
       y = "Species",
       caption = "Graphic: @JoshuaFeIdman") +
  guides(col = FALSE, size = FALSE) +
  facet_wrap(~gender, scales = "free_x") +
  scale_color_manual(values = c("#0072B2","#009E73","#56B4E9","#999999",
                                "#CC79A7","#D55E00","#E69F00","#F0E442"))