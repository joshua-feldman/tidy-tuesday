library(tidyverse)
library(treemap)

df <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv") %>% 
  mutate(n_phds = ifelse(is.na(n_phds), 0, n_phds))

df %>% 
  group_by(year, broad_field) %>% 
  summarise(n_phds = sum(n_phds)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = n_phds, col = broad_field))

df_2008 <- df %>% 
  filter(year == 2008) %>% 
  select(field, n_phds)

df_2017 <- df %>% 
  filter(year == 2017)

df_comp <- df_2008 %>% 
  left_join(df_2017, by = "field") %>% 
  mutate(diff = n_phds.y - n_phds.x)

treeMapCoordinates <- treemapify(df_2017,
                                 area = "n_phds",
                                 subgroup = "broad_field",
                                 subgroup2 = "major_field",
                                 subgroup3 = "field")

df_2017$broad_field <- toupper(df_2017$broad_field)

# Treemap
treemap(df_2017,
        index = c("broad_field", "major_field", "field"),
        vSize = "n_phds",
        fontfamily.labels = "Lato",
        fontsize.labels = c(18, 14, 8),
        overlap.labels = 0,
        border.lwds = c(3, 2, 1),
        bg.labels = 255,
        title = "")
