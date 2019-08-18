library(tidyverse)

df <- read.csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-05-07/student_teacher_ratio.csv")

df_income <- df %>% 
  filter(str_detect(country, "income")) %>% 
  mutate(country = factor(country, levels = c("Low income countries", "Lower middle income countries",
                                              "Middle income countries", "Upper middle income countries",
                                              "High income countries"))) %>% 
  mutate(indicator = factor(indicator, levels = c("Pre-Primary Education", "Primary Education",
                                                  "Lower Secondary Education", "Secondary Education",
                                                  "Upper Secondary Education")))

df_income %>% 
  filter(year == 2016) %>% 
  mutate(flag = ifelse(indicator == "Primary Education", 1, 0)) %>% 
  mutate(student_ratio_label = paste(round(student_ratio), ":1", sep = "")) %>% 
  ggplot(aes(indicator, student_ratio, fill = country, alpha = flag, label = student_ratio_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(color = country), position = position_dodge(width = 0.9),
            fontface = "bold", vjust = -0.5, size = 5) +
  labs(title = "Class sizes in primary schools more than double in low-income vs high-income countries",
       subtitle = "Global student-to-teacher ratios (UNESCO Institute of Statistics, 2016)",
       x = "Stage of education",
       y = "Student-to-teacher ratio",
       caption = "Graphic: @JoshuaFeIdman\nSource: #TidyTuesday") +
  theme_dark(base_size = 20, base_family = "Lato") +
  theme(axis.text = element_text(color = "white", family = "Open Sans"),
        axis.ticks = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(20,0,0,0)),
        axis.title.y = element_text(margin = margin(0,20,0,0)),
        panel.background = element_rect(fill = "#1a1a1a"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#1a1a1a"),
        plot.title = element_text(face = "bold"),
        text = element_text(color = "white")) +
  guides(fill = FALSE, color = FALSE, alpha = FALSE) +
  annotate("text", label = "Low-income countries",
           x = 3.15, y = 35, col = "#08519c", family = "Open Sans", fontface = "bold",
           size = 7) +
  annotate("text", label = "High-income countries",
           x = 4.87, y = 35, col = "white", family = "Open Sans", fontface = "bold",
           size = 7) +
  geom_segment(x = 3.75, xend = 3.85, y = 35, yend = 35,
               size = 2, col = "#08519c") +
  geom_segment(x = 3.85, xend = 3.95, y = 35, yend = 35,
               size = 2, col = "#3082bd") +
  geom_segment(x = 3.95, xend = 4.05, y = 35, yend = 35,
               size = 2, col = "#6bafd6") +
  geom_segment(x = 4.05, xend = 4.15, y = 35, yend = 35,
               size = 2, col = "#bed7e7") +
  geom_segment(x = 4.15, xend = 4.25, y = 35, yend = 35,
               size = 2, col = "white", arrow = arrow(length = unit(0.5, "cm"))) + 
  scale_alpha_continuous(range = c(0.5, 1)) +
  scale_color_brewer(direction = -1, palette = "Blues") +
  scale_fill_brewer(direction = -1, palette = "Blues")
