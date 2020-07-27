library(tidyverse)
library(png)
library(grid)
library(patchwork)
library(ggimage)
library(ggtext)

penguin_theme <- theme(
  panel.background = element_rect(fill = "#fffafa", color = "#fffafa"),
  panel.grid.major = element_line(color = "#fffafa"),
  panel.grid.minor = element_line(color = "#fffafa"),
  plot.background = element_rect(fill = "#fffafa", color = "#fffafa"),
  plot.title = element_text(hjust = 0.5),
  text = element_text(family = "Iceberg"))

theme_set(penguin_theme)

img <- readPNG("penguins.png")
g <- rasterGrob(img, interpolate=TRUE)

penguin_cols <- c("#ff7204", "#c15bcc", "#087075")

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

p1 <- df %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, col = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = penguin_cols) +
  guides(col = FALSE) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)")

p2 <- df %>% 
  ggplot(aes(flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = penguin_cols) +
  guides(fill = FALSE) +
  labs(x = "Flipper length (mm)",
       y = "Density")

p3 <- df %>% 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = penguin_cols) +
  guides(fill = FALSE) +
  labs(x = "Body mass (g)",
       y = "Density")

wrap_elements(g) / p1 / (p2 | p3) +
  plot_annotation(title = "Meet the Palmer Penguins",
                  subtitle = "The <strong><span style='color:#087075'>Gentoo</span></strong> is heavy with big flippers. The <strong><span style='color:#ff7204'>Adélie</span></strong> has a deep but short bill.<br>And the <strong><span style='color:#c15bcc'>Chinstrap</span></strong>? Well, the Chinstrap is somewhere in-between.",
                  caption = "Graphic: @JoshuaFeIdman",
                  theme = theme(panel.background = element_rect(fill = "#fffafa", color = "#fffafa"),
                                plot.background = element_rect(fill = "#fffafa", color = "#fffafa"),
                                plot.title = element_text(hjust = 0.5, size = 30, family = "Iceberg", face = "bold", margin = margin(0, 0, 10, 0)),
                                plot.subtitle = element_markdown(hjust = 0.5, size = 15, family = "Iceberg"),
                                text = element_text(family = "Iceberg")))
