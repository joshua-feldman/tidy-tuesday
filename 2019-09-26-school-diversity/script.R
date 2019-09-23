library(tidyverse)
library(vegan)
library(RColorBrewer)
library(ggalt)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

source("~/theme.R")

df_reduced <- df[6:10]
df_reduced$diversity <- diversity(df_reduced)
df$diversity <- df_reduced$diversity

states <- data.frame(ST = state.abb, state.name) %>% 
  mutate(ST = as.character(ST)) %>% 
  mutate(state.name = as.character(state.name))

states[nrow(states) + 1,] = c("DC", "Washington DC")

df_subplot <- df %>% 
  filter(ST == "DC") %>% 
  select(SCHOOL_YEAR, ST, AIAN, Asian, Black, Hispanic, White) %>% 
  group_by(SCHOOL_YEAR, ST) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% 
  gather(ethnicity, percent, -SCHOOL_YEAR, -ST)

plot1 <- df_subplot %>% 
  filter(SCHOOL_YEAR == '1994-1995') %>% 
  mutate(percent = percent / 100) %>% 
  ggplot(aes(ethnicity, percent, fill = ethnicity)) +
  geom_col() +
  labs(title = "Typical DC school in 1994") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.background = element_rect(color = "#666666", fill = "#666666"),
        panel.background = element_rect(color = "#666666", fill = "#666666"),
        panel.grid.major = element_line(color = "#808080", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#808080", size = rel(0.25)),
        text = element_text(size = 10, face = "bold"),
        title = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot2 <- df_subplot %>% 
  filter(SCHOOL_YEAR == '2016-2017') %>% 
  mutate(percent = percent / 100) %>% 
  ggplot(aes(ethnicity, percent, fill = ethnicity)) +
  geom_col() +
  labs(title = "Typical DC school in 2016") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = FALSE) +
  coord_flip() +
  theme(plot.background = element_rect(color = "#666666", fill = "#666666"),
        panel.background = element_rect(color = "#666666", fill = "#666666"),
        panel.grid.major = element_line(color = "#808080", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#808080", size = rel(0.25)),
        text = element_text(size = 10, face = "bold"),
        title = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot1 <- ggplotGrob(plot1)
plot2 <- ggplotGrob(plot2)
  
cols <- brewer.pal(9, "Blues")

main_plot <- df %>% 
  filter(ST != "ID") %>% 
  group_by(SCHOOL_YEAR, ST) %>% 
  summarise(diversity = mean(diversity)) %>% 
  spread(SCHOOL_YEAR, diversity) %>% 
  mutate(diff = `2016-2017` - `1994-1995`) %>% 
  left_join(states) %>% 
  mutate(state.name = reorder(state.name, -`1994-1995`)) %>% 
  ggplot(aes(x=`1994-1995`, xend=`2016-2017`, y = state.name, group = state.name)) +
  geom_dumbbell(size = 1, color = cols[3], colour_xend = cols[6]) +
  labs(title = "Ethnic diversity in United States schools: Comparing 1994–95 to 2016–17",
       subtitle = "As measured by the Shannon index, ethnic diversity in schools increased in all US states from 1994–95 to\n2016–17. The largest change has been in Washington DC, where diversity has increased by 106%.",
       x = "Ethnic diversity",
       y = "State",
       caption = "Graphic: Joshua Feldman") +
  theme(axis.text.y = element_text(size = 11))

main_plot +
  annotation_custom(grob = plot1,
                    xmin = 0.6, xmax = 0.82,
                    ymin = 27, ymax = 47) +
  annotation_custom(grob = plot2,
                    xmin = 0.83, xmax = 1.05,
                    ymin = 27, ymax = 47)
  