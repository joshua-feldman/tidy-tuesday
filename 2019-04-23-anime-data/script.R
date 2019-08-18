library(tidyverse)
library(tidytext)
library(corrr)

df_anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

df_network <- df_anime %>% 
  select(name, genre) %>% 
  distinct()

df_network <- df_network %>% 
  mutate(flag = rep(1, nrow(df_network))) %>% 
  spread(genre, flag) %>% 
  select(-name, -`<NA>`)
  
df_network[is.na(df_network)] <- 0

df_network %>% 
  correlate() %>% 
  network_plot(min_cor = 0, colors = c("red", "white", "green"), legend = FALSE) +
  labs(title = "Common genre combinations in anime and manga",
       subtitle = "Network plot of genres from the MyAnimeList (MAL) database",
       caption = "Graphic: @JoshuaFeIdman\nSource: #TidyTuesday",
       color = "white") +
  theme_dark(base_size = 20, base_family = "Lato") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#7f7f7f"),
        plot.title = element_text(face = "bold"),
        text = element_text(color = "white")) +
  guides(color = FALSE)