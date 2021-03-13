library(tidyverse)

setwd("~/tidy-tuesday/2021-03-09-bechdel")

movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

movies_ext <- movies %>% 
  filter(!is.na(genre)) %>% 
  mutate(binary = ifelse(binary == "PASS", 1, 0)) %>% 
  mutate(action = ifelse(str_detect(genre, "Action"), 1, 0)) %>% 
  mutate(adventure = ifelse(str_detect(genre, "Adventure"), 1, 0)) %>% 
  mutate(animation = ifelse(str_detect(genre, "Animation"), 1, 0)) %>% 
  mutate(biography = ifelse(str_detect(genre, "Biography"), 1, 0)) %>% 
  mutate(comedy = ifelse(str_detect(genre, "Comedy"), 1, 0)) %>% 
  mutate(crime = ifelse(str_detect(genre, "Crime"), 1, 0)) %>% 
  mutate(documentary = ifelse(str_detect(genre, "Documentary"), 1, 0)) %>% 
  mutate(drama = ifelse(str_detect(genre, "Drama"), 1, 0)) %>% 
  mutate(family = ifelse(str_detect(genre, "Family"), 1, 0)) %>% 
  mutate(fantasy = ifelse(str_detect(genre, "Fantasy"), 1, 0)) %>% 
  mutate(horror = ifelse(str_detect(genre, "Horror"), 1, 0)) %>% 
  mutate(musical = ifelse(str_detect(genre, "Musical"), 1, 0)) %>% 
  mutate(mystery = ifelse(str_detect(genre, "Mystery"), 1, 0)) %>% 
  mutate(romance = ifelse(str_detect(genre, "Romance"), 1, 0)) %>% 
  mutate(scifi = ifelse(str_detect(genre, "Sci-Fi"), 1, 0)) %>% 
  mutate(thriller = ifelse(str_detect(genre, "Thriller"), 1, 0)) %>% 
  mutate(western = ifelse(str_detect(genre, "Western"), 1, 0))

movies_filtered <- movies_ext %>% 
  select(binary, action:western) %>% 
  gather(genre, flag, -binary) %>% 
  filter(flag == 1)

movies_agg <- movies_filtered %>% 
  group_by(genre) %>% 
  summarise(score = mean(binary),
            n = n()) %>% 
  ungroup() %>% 
  mutate(se = sqrt((score * (1 - score)) / n),
         ci_upper = score + se * 1.96,
         ci_lower = score - se * 1.96,
         ci_upper90 = score + se * 1.645,
         ci_lower90 = score - se * 1.645) %>% 
  mutate(ci_lower = ifelse(ci_lower < 0, 0, ci_lower)) %>% 
  mutate(ci_lower90 = ifelse(ci_lower90 < 0, 0, ci_lower90))

explainer <- glue::glue(paste0("<b>What is the Bechdel Test?</b><br>The Bechdel Test measures female representation in cinema.<br>For a film to pass the test, it needs to meet three criteria:<br><br>1) it has to have at least two women in it, who<br>2) talk to each other, about<br>3) something besides a man"))

movies_agg %>% 
  filter(!genre %in% c("documentary")) %>% 
  mutate(genre = str_to_title(genre)) %>% 
  mutate(genre = reorder(genre, score)) %>% 
  ggplot(aes(genre, score, group = 1)) +
  # geom_image(aes(image = img), size=.05) +
  geom_crossbar(aes(min = ci_lower90, ymax = ci_upper90, fill = score), color = "#555555") +
  geom_crossbar(aes(min = ci_lower, ymax = ci_upper, fill = score), alpha = 0.5, color =  "#555555") +
  geom_point(aes(color = score), size = 3) +
  geom_label(aes(label = genre), family = "Permanent Marker", label.size = NA, color = "black", fontface = "bold") +
  annotate("richtext", size = 5,  x = 13, y = 0, label = explainer, label.size = NA, fill = NA, color = "white",
           family = "Raleway", hjust = 0) +
  annotate("rect", xmin = 2, xmax = 3, ymin = .625, ymax = .775, fill = "white") +
  annotate("rect", xmin = 2, xmax = 3, ymin = .6, ymax = .8, fill = "white", alpha = 0.5) +
  annotate("segment", x = 2, xend = 3, y = .7, yend = .7, size = 1, color = "#555555") +
  annotate("text", x = 3.5, y = .7, color = "white", label = "Point estimate", family = "Raleway", fontface = "bold") +
  annotate("text", x = 1.5, y = .7, color = "white", label = "90% Confidence Interval", family = "Raleway", fontface = "bold") +
  annotate("text", x = 1, y = .7, color = "white", label = "95% Confidence Interval", family = "Raleway", fontface = "bold") +
  annotate("segment", x = 1.5, xend = 1.5, y = .76, yend = .775, color = "white", arrow = arrow(length = unit(0.1,"cm"))) +
  annotate("segment", x = 1, xend = 1, y = .76, yend = .8, color = "white", arrow = arrow(length = unit(0.1,"cm"))) +
  annotate("segment", x = 1, xend = 1, y = .64, yend = .6, color = "white", arrow = arrow(length = unit(0.1,"cm"))) +
  annotate("segment", x = 1.5, xend = 1.5, y = .64, yend = .625, color = "white", arrow = arrow(length = unit(0.1,"cm"))) +
  labs(title = "Percentage of films that pass the Bechdel Test by genre",
       subtitle = "According to an analysis of 1,592 films, musicals pass the Bechdel Test most frequently, while Westerns do so just 17% of the time.",
       x = "Genre",
       y = "Percentage of films that pass the Bechdel Test",
       caption = "Graphic: Joshua Feldman") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 18) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "#666666"),
        panel.grid.minor.x = element_line(color = "#666666"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0, size = 17, margin = margin(0, 0, 20, 0)),
        axis.text.x = element_text(color = "white"),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        panel.background = element_rect(fill = "#555555", color = "#555555"),
        plot.background = element_rect(fill = "#555555", color = "#555555"),
        plot.caption = element_text(margin = margin(10, 0, 0, 0)),
        text = element_text(color = "white", family = "Roboto")) +
  guides(color = FALSE, fill = FALSE) +
  scale_color_gradient2(high = "#71CA97", low = "#ff7f7f",
                        midpoint = 0.5) +
  scale_fill_gradient2(high = "#71CA97", low = "#ff7f7f",
                       midpoint = 0.5) +
  coord_flip()

height <- 8

ggsave("graphic.png", height = height, width = height * (16/9))
