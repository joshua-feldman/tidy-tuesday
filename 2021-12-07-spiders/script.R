library(tidyverse)
library(ggExtra)
library(showtext)
library(ggforce)

spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

x <- spiders %>% count(family) %>% arrange(-n)
x$height <- sqrt(x$n * 2) # So we size the polygons by area rather than height
x$height_scaled <- 5 * ((x$height - min(x$height)) / (max(x$height) - min(x$height)))

top_spiders <- top_n(x, 8, n)

title_font <- "Cabin"

font_add_google(title_font)
font_add_google("Ubuntu")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

r <- -5:5

x1 <- r * cos(pi * 0)
x2 <- r * cos(pi * 0.25)
x3 <- r * cos(pi * 0.5)
x4 <- r * cos(pi * 0.75)
x5 <- r * cos(pi * 1)
x6 <- r * cos(pi * 1.25)
x7 <- r * cos(pi * 1.50)
x8 <- r * cos(pi * 1.75)

y1 <- r * sin(pi * 0)
y2 <- r * sin(pi * 0.25)
y3 <- r * sin(pi * 0.5)
y4 <- r * sin(pi * 0.75)
y5 <- r * sin(pi * 1)
y6 <- r * sin(pi * 1.25)
y7 <- r * sin(pi * 1.50)
y8 <- r * sin(pi * 1.75)

df <- data.frame(
  r = r,
  order = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75),
  x = c(x1, x2, x3, x4, x5, x6, x7, x8),
  y = c(y1, y2, y3, y4, y5, y6, y7, y8)
)

triangle1 <- data.frame(x = c(0, 0, top_spiders$height_scaled[1] * sin(pi * 0.25)),
                        y = c(0, top_spiders$height_scaled[1], top_spiders$height_scaled[1] * cos(pi * 0.25)))

triangle2 <- data.frame(x = c(0, top_spiders$height_scaled[2], top_spiders$height_scaled[2] * sin(pi * 0.25)),
                        y = c(0, 0, top_spiders$height_scaled[2] * cos(pi * 0.25)))

triangle3 <- data.frame(x = c(0, top_spiders$height_scaled[3], top_spiders$height_scaled[3] * sin(pi * 0.25)),
                        y = c(0, 0, -top_spiders$height_scaled[3] * cos(pi * 0.25)))

triangle4 <- data.frame(x = c(0, 0, top_spiders$height_scaled[4] * sin(pi * 0.25)),
                        y = c(0, -top_spiders$height_scaled[4], -top_spiders$height_scaled[4] * cos(pi * 0.25)))

triangle5 <- data.frame(x = c(0, 0, -top_spiders$height_scaled[5] * sin(pi * 0.25)),
                        y = c(0, -top_spiders$height_scaled[5], -top_spiders$height_scaled[5] * cos(pi * 0.25)))

triangle6 <- data.frame(x = c(0, -top_spiders$height_scaled[6], -top_spiders$height_scaled[6] * sin(pi * 0.25)),
                        y = c(0, 0, -top_spiders$height_scaled[6] * cos(pi * 0.25)))

triangle7 <- data.frame(x = c(0, -top_spiders$height_scaled[7], -top_spiders$height_scaled[7] * sin(pi * 0.25)),
                        y = c(0, 0, top_spiders$height_scaled[7] * cos(pi * 0.25)))

triangle8 <- data.frame(x = c(0, 0, -top_spiders$height_scaled[8] * sin(pi * 0.25)),
                        y = c(0, top_spiders$height_scaled[8], top_spiders$height_scaled[8] * cos(pi * 0.25)))

colors <- RColorBrewer::brewer.pal(8, "Set3")
radians_to_degrees <- function(radians) {return(radians * (180 / pi))}

df %>% 
  ggplot(aes(x, y, group = r)) +
  geom_polygon(data = triangle1, aes(x = x, y = y), fill = colors[1], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle2, aes(x = x, y = y), fill = colors[2], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle3, aes(x = x, y = y), fill = colors[3], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle4, aes(x = x, y = y), fill = colors[4], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle5, aes(x = x, y = y), fill = colors[5], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle6, aes(x = x, y = y), fill = colors[6], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle7, aes(x = x, y = y), fill = colors[7], inherit.aes = FALSE, alpha = 0.75) +
  geom_polygon(data = triangle8, aes(x = x, y = y), fill = colors[8], inherit.aes = FALSE, alpha = 0.75) +
  # geom_point(color = "white", alpha = 0.1) +
  geom_path(color = "white", alpha = 0.5) +
  annotate("segment", x = min(df$x), y = 0, xend = max(df$x), yend = 0, color = "white", alpha = 0.5) +
  annotate("segment", x = 0, y = min(df$y), xend = 0, yend = max(df$y), color = "white", alpha = 0.5) +
  annotate("segment", x = min(5 * cos(pi * 0.25)), xend = max(5 * cos(pi * 1.25)),
                   y = min(5 * sin(pi * 0.25)), yend = max(5 * sin(pi * 1.25)), color = "white", alpha = 0.5) +
  annotate("segment", x = min(5 * cos(pi * 0.75)), xend = max(5 * cos(pi * 1.75)),
                   y = min(5 * sin(pi * 0.75)), yend = max(5 * sin(pi * 1.75)), color = "white", alpha = 0.5) +
  annotate("text", label = paste0(top_spiders$family[1], "\n", top_spiders$n[1]),
           x = 5 * cos(pi * 0.375), y = 5 * sin(pi * 0.375), angle = -22.5, color = colors[1], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[2], "\n", top_spiders$n[2]), 
           x = 5 * cos(pi * 0.125), y = 5 * sin(pi * 0.125), angle = -67.5,color = colors[2], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[3], "\n", top_spiders$n[3]),
           x = 5 * cos(pi * 0.125), y = -5 * sin(pi * 0.125), angle = -112.5 - 180, color = colors[3], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[4], "\n", top_spiders$n[4]),
           x = 5 * cos(pi * 0.375), y = -5 * sin(pi * 0.375), angle = -157.5 - 180, color = colors[4], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[5], "\n", top_spiders$n[5]),
           x = -5 * cos(pi * 0.375), y = -5 * sin(pi * 0.375), angle = 157.5 - 180, color = colors[5], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[6], "\n", top_spiders$n[6]),
           x = -5 * cos(pi * 0.125), y = -5 * sin(pi * 0.125), angle = 112.5 - 180, color = colors[6], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[7], "\n", top_spiders$n[7]),
           x = -5 * cos(pi * 0.125), y = 5 * sin(pi * 0.125), angle = 67.5, color = colors[7], family = "Ubuntu") +
  annotate("text", label = paste0(top_spiders$family[8], "\n", top_spiders$n[8]),
           x = -5 * cos(pi * 0.375), y = 5 * sin(pi * 0.375), angle = 22.5,color = colors[8], family = "Ubuntu") +
  geom_curve(x = 0, xend = -0.25, y = 0.28, yend = 0.3, curvature = -0.4, size = 0.3) +
  geom_curve(x = 0, xend = 0.25, y = 0.28, yend = 0.3, curvature = 0.4, size = 0.3) +
  geom_curve(x = 0, xend = 0.1, y = 0.3, yend = 0.4, curvature = 0.4, size = 0.3) +
  geom_curve(x = 0, xend = -0.1, y = 0.3, yend = 0.4, curvature = -0.4, size = 0.3) +
  geom_curve(x = 0, xend = -0.25, y = 0.15, yend = 0.1, curvature = 0.4, size = 0.3) +
  geom_curve(x = 0, xend = 0.25, y = 0.15, yend = 0.1, curvature = -0.4, size = 0.3) +
  geom_curve(x = 0, xend = 0.25, y = 0.1, yend = 0, curvature = -0.4, size = 0.3) +
  geom_curve(x = 0, xend = -0.25, y = 0.1, yend = 0, curvature = 0.4, size = 0.3) +
  geom_ellipse(aes(x0 = 0, y0 = 0, b = 0.15, a = 0.1, angle = 0), fill = "black",colour = "white") +
  annotate("point", x = 0, y = 0.22, size = 3, color = "white", pch = 21, fill = "black") +
  theme_void() +
  labs(title = "The eight largest families of spider by number of species",
       caption = "Graphic: Joshua Feldman") +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(color = "#262626", fill = "#262626"),
    plot.title = element_text(color = "white", hjust = 0.5, family = title_font, face = "bold", size = 20, margin = margin(10, 0, -20, 0)),
    plot.caption = element_text(color = "white", hjust = 0.5, family = title_font, size = 10)
  )

