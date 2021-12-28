library(tidyverse)
library(extrafont)

extrafont::font_import()
extrafont::loadfonts()
extrafont::fonts()

img <- readPNG("~/Desktop/logo.png")
g <- grid::rasterGrob(img, interpolate=TRUE)

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv') %>% 
  mutate(milk = recode(milk, "0" = "none", "1" = "nonfat", "2" = "2%", "3" = "soy", "4" = "coconut", "5" = "whole")) %>% 
  mutate(whip = recode(whip, "0" = "No cream", "1" = "Cream"))

# Source: https://creative.starbucks.com/color/
colors <- c(starbucks_green = "#006140",
            accent_green = "#01754a",
            light_green = "#d4e8e1",
            house_green = "#1e3832",
            black = "#000000",
            warm_neutral = "#f2f0ea",
            cool_neutral = "#f9f9f9",
            white = "#ffffff")

# Choices:
# 1. Product
# 2. Size
# 3. Type of milk
# 4. Whip or no whip

df_filtered <- df %>% 
  filter(size %in% c("short", "tall", "grande", "venti")) %>% 
  filter(milk != "none")

mean(df_filtered$calories)

step1 <- df_filtered %>% 
  group_by(size) %>% 
  summarise(step1_calories = mean(calories)) %>% 
  ungroup()

step2 <- df_filtered %>% 
  group_by(size, milk) %>% 
  summarise(step2_calories = mean(calories)) %>% 
  ungroup()

step3 <- df_filtered %>% 
  group_by(size, milk, whip) %>% 
  summarise(step3_calories = mean(calories)) %>% 
  ungroup()

final <- step3 %>% 
  left_join(step2, by = c("size" = "size", "milk" = "milk")) %>% 
  left_join(step1, by = c("size" = "size")) %>% 
  mutate(step0_calories = mean(df_filtered$calories)) %>% 
  mutate(group = paste0(size, "-", milk, "-", whip)) %>% 
  gather(key, value, -group, -size, -milk, -whip) %>% 
  mutate(label = ifelse(key == "step0_calories", size,
                 ifelse(key == "step1_calories", milk,
                 ifelse(key == "step2_calories", whip, "No label")))) %>% 
  mutate(label = toupper(label)) %>%
  mutate(key = factor(key, levels = c("step3_calories", "step2_calories",
                                      "step1_calories", "step0_calories"))) %>% 
  mutate(order = case_when(
    size == "short" ~ 1,
    size == "tall" ~ 2,
    size == "grande" ~ 3,
    size == "venti" ~ 4
  ))

final %>% 
  mutate(size = as.factor(size)) %>% 
  mutate(size = factor(size, levels = c("short", "tall", "grande", "venti"))) %>% 
  filter(!key %in% c("step0_calories")) %>%
  ggplot(aes(key, value, color = size)) +
  # geom_line(color = colors['warm_neutral']) +
  geomtextpath::geom_textline(data = filter(final, key %in% c("step1_calories", "step2_calories")),
                              aes(group = group, label = label), family = "Santana") +
  geomtextpath::geom_textline(data = filter(final, key %in% c("step2_calories", "step3_calories")),
                              aes(group = group, label = label), family = "Santana") +
  geom_point(size = 3, alpha = 0.5) +
  # geom_label(hjust = 0.5, check_overlap = TRUE, family = "Santana-Black") +
  labs(title = "STARBUCKS CALORIE COUNTER",
       subtitle = "This chart shows you the average number of calories in a Starbucks drink after you pick\nthe size, the type of milk, and whether or not you want whipped cream.",
       y = "Calories",
       caption = "GRAPHIC: JOSHUA FELDMAN") +
  # scale_y_continuous(limits = c(50, 650), breaks = c(100, 200, 300, 400, 500)) +
  # annotation_custom(g, xmin = 2, ymin = 25, xmax = 3, ymax = 75) +
  guides(shape = FALSE, color = FALSE) + 
  facet_wrap(~factor(toupper(size), levels = c("SHORT", "TALL", "GRANDE", "VENTI")), ncol = 4, nrow = 1) +
  # scale_y_discrete(limits = levels(final$key)) +
  # annotate("label", x = 1, y = 550, color = colors['house_green'], family = "Santana-Black", hjust = 0, size = 6, label = "STEP 1: SELECT YOUR SIZE") +
  # annotate("label", x = 2, y = 550, color = colors['house_green'], family = "Santana-Black", hjust = 0, size = 6, label = "STEP 2: SELECT YOUR MILK") +
  # annotate("label", x = 3, y = 550, color = colors['house_green'], family = "Santana-Black", hjust = 0, size = 6, label = "STEP 3: WHIPPED CREAM?") +
  theme(axis.text = element_text(color = "white", size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        legend.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        legend.key = element_rect(fill = colors['house_green'], color = colors['house_green']),
        legend.text = element_text(color = "white"),
        panel.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = colors['warm_neutral'], size = 0.1),
        panel.grid.minor = element_blank(),
        plot.margin = margin(c(20, 20, 20, 20)),
        plot.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        plot.title = element_text(face = "bold", family = "Santana-Black", size = 30, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", family = "Santana", size = 20, hjust = 0.5, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(face = "bold", family = "Santana", size = 12, hjust = 0.5),
        text = element_text(color = "white", family = "Santana"),
        strip.text = element_text(color = colors['house_green'], size = 14, family = "Santana-Black"),
        strip.background = element_rect(fill = colors['cool_neutral'])) +
  scale_color_manual(values = c("#d4e8e1", "#dd91c0", "#efcdbb", "#efe0af"))


  df_final <- expand_grid(df_filtered_size, df_filtered_milk, df_filtered_whip) %>% 
  mutate(calories_size_milk = calories_size + calories_milk) %>% 
  mutate(calories_size_milk_whip = calories_size + calories_milk + calories_whip) %>% 
  mutate(group = paste0(size, "-", milk, "-", whip))




x %>% 
  ggplot(aes(key, calories, group = value)) +
  geom_line()

x %>% 
  ggplot()

# 2. Size
df %>% 
  filter(size %in% c("short", "tall", "grande", "venti")) %>% 
  mutate(size = factor(size, levels = c("short", "tall", "grande", "venti"))) %>% 
  group_by(size) %>% 
  summarise(calories = mean(calories)) %>% 
  ungroup()

# 3. Milk
df %>% 
  group_by(milk) %>% 
  summarise(calories = mean(calories)) %>% 
  ungroup()

# 3. Whip
df %>% 
  group_by(whip) %>% 
  summarise(calories = mean(calories)) %>% 
  ungroup()




summary(lm(calories ~ milk + whip + size, df))


df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv') %>% 
  mutate(milk = recode(milk, "0" = "none", "1" = "nonfat", "2" = "2%", "3" = "soy", "4" = "coconut", "5" = "whole"))

whip_availability <- df %>% 
  group_by(product_name) %>% 
  summarise(whip_availability = sum(whip)) %>% 
  ungroup() %>% 
  filter(whip_availability > 0)

x <- df %>% 
  select(product_name, size, whip, milk) %>% 
  group_by(product_name, size, whip, milk) %>% 
  mutate(n = n())

df_arranged <- df %>% 
  arrange(product_name, milk, size, whip)

df %>% 
  mutate(product_name = reorder(product_name, calories)) %>% 
  ggplot(aes(as.factor(milk), product_name, size = calories)) +
  geom_point()
  
df %>% 
  filter(size %in% c("short", "tall", "grande", "venti")) %>% 
  mutate(size = factor(size, levels = c("short", "tall", "grande", "venti"))) %>% 
  filter(product_name %in% whip_availability$product_name) %>% 
  ggplot(aes(calories, y = as.factor(size), fill = as.factor(whip))) +
  geom_violin(color = NA) +
  labs(title = "EXAMPLE TITLE",
       subtitle = "Example subtitle") +
  scale_fill_manual(values = c(unname(colors[6]), unname(colors[3]))) +
  theme(axis.text = element_text(color = "white"),
        legend.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        legend.key = element_rect(fill = colors['house_green'], color = colors['house_green']),
        legend.text = element_text(color = "white"),
        panel.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = colors['house_green'], color = colors['house_green']),
        plot.title = element_text(face = "bold", family = "Santana-Black"),
        text = element_text(color = "white", family = "Santana"))
