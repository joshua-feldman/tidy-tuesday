library(tidyverse)
library(emoGG)
library(gganimate)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv") %>% 
  mutate(ped_count = as.numeric(ped_count)) %>% 
  mutate(date_new = str_sub(date, 1, 10)) %>% 
  mutate(time_new = format(strptime(str_sub(date, 11, 22), "%I:%M:%S %p"),
                           format = "%H:%M:%S")) %>% 
  mutate(time_new = as.numeric(substr(time_new, 1, 2)))

df_aggbycrossing <- df %>% 
  filter(!is.na(bike_count), !is.na(ped_count)) %>% 
  group_by(crossing, time_new) %>% 
  summarise(bike_count = mean(bike_count, na.rm = TRUE),
            ped_count = mean(ped_count, na.rm = TRUE)) %>% 
  gather(key, value, -crossing, -time_new)

ped <- df_aggbycrossing %>% filter(key == "ped_count")
bike <- df_aggbycrossing %>% filter(key == "bike_count")

ggplot() +
  geom_emoji(data = ped, aes(factor(crossing), value), emoji = "1f6b6", size = 0.1) +
  geom_emoji(data = bike, aes(factor(crossing), value), emoji = "1f6b4", size = 0.1) +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = "Lato"),
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0,20,0,0)),
        axis.title.x = element_text(margin = margin(20,0,0,0))) +
  guides(shape = FALSE, col = FALSE) +
  labs(title = "Time of day in Seattle, Washington: {paste(round(frame_time), ':00', sep = '')}",
       subtitle = "Average number of bikes and pedestrians in transit",
       x = "Crossing",
       y = "Average",
       caption = "Source: #TidyTuesday\nGraphic: @JoshuaFeIdman") +
  transition_time(time_new) +
  ease_aes('linear')



