library(tidyverse)

th <- theme_minimal() +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

theme_set(th)

emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors <- emperors %>% 
  mutate(dynasty = factor(dynasty, levels = c("Julio-Claudian", "Flavian", "Nerva-Antonine", "Severan",
                                              "Gordian", "Constantinian", "Valentinian", "Theodosian"))) %>% 
  mutate(reign_start_new = as.numeric(substr(reign_start, 1, 4))) %>% 
  mutate(reign_end_new = as.numeric(substr(reign_end, 1, 4)))

emperors$reign_start_new[1] <- -emperors$reign_start_new[1]

emperors <- emperors %>% 
  arrange(reign_start_new, index) %>% 
  mutate(index = row_number())

emperors %>% 
  mutate(name = reorder(name, rev(index))) %>% 
  ggplot(aes(reign_start_new, name, col = dynasty)) +
  geom_errorbarh(aes(xmin = reign_start_new, xmax = reign_end_new), lwd = 1) +
  geom_hline(yintercept = 20.5, linetype = "dashed") +
  labs(title = "The reign of Roman Emperors by dynasty",
       subtitle = "From Augustus (26BC–14AD) to Theodosius I (379AD–395AD)",
       x = "Year",
       y = NULL,
       caption = "Source: Wikipedia\nGraphic: @JoshuaFeIdman") +
  annotate("text", label = "Principate era", x = 0, y = 22.5, family = "Raleway",
           hjust = 0, size = 4, fontface = "bold") +
  annotate("text", label = "Dominate era", x = 0, y = 18.5, family = "Raleway",
           hjust = 0, size = 4, fontface = "bold") +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "ivory1"),
        legend.title = element_text(face = "bold", size = 14)) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = "Dynasty"))




