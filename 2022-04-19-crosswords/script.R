library(tidyverse)
library(showtext)

font_add_google("Gentium Book Basic")
showtext_auto()

times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

# Most common words
word_list <- times %>% 
  filter(!is.na(answer)) %>% 
  count(answer) %>% 
  arrange(-n) %>% 
  top_n(20, n) %>%
  mutate(nchar = nchar(answer)) %>%
  filter(nchar >= 3) %>% 
  mutate(letter1 = str_sub(answer, 1, 1)) %>%
  mutate(letter2 = str_sub(answer, 2, 2)) %>%
  mutate(letter3 = str_sub(answer, 3, 3)) %>%
  mutate(letter4 = str_sub(answer, 4, 4)) %>%
  mutate(letter5 = str_sub(answer, 5, 5))

# Use this to find example definitions
definition_list <- times %>% 
  filter(answer %in% word_list$answer) %>%
  group_by(answer) %>%
  sample_n(1)

# Input top words into online tool (e.g. crosswordlabs.com) to find out how to arrange them
# Replicate structure below

row1 <- c(rep("#", 4), "T", "I", "E", rep("#", 6))
row2 <- c(rep("#", 2), "O", "N", "E", "#", "A", rep("#", 6))
row3 <- c(rep("#", 2), "M", "#", "A", "I", "R", rep("#", 6))
row4 <- c(rep("#", 2), "E", rep("#", 10))
row5 <- c("R", "I", "G", "H", "T", rep("#", 8))
row6 <- c("U", "#", "A", "#", "W", "#", "S", rep("#", 5), "T")
row7 <- c("N", rep("#", 3), "O", "U", "T", "#", "A", "M", "I", "G", "O")
row8 <- c(rep("#", 6), "A", "S", "S", "#", "C", "#", "P")
row9 <- c(rep("#", 3), "F", "A", "I", "R", "#", "H", "#", "E", rep("#", 2))
row10 <- c(rep("#", 5), "L", rep("#", 7))
row11 <- c(rep("#", 2), "W", "E", "L", "L", rep("#", 7))
row12 <- c(rep("#", 3), "X", rep("#", 9))
row13 <- c(rep("#", 3), "T", rep("#", 9))
row14 <- c(rep("#", 3), "R", rep("#", 9))
row15 <- c(rep("#", 3), "A", rep("#", 9))

df <- do.call(rbind, mget(ls(pattern = "row")))
rownames(df) <- str_remove_all(rownames(df), "row")

df_ordered <- df[ order(as.numeric(row.names(df))), ] %>%
  as.data.frame() %>%
  mutate(row = row_number()) %>% 
  gather(column, value, -row) %>% 
  mutate(column = as.numeric(str_remove_all(column, "V"))) %>%
  mutate(valid = ifelse(value %in% LETTERS, 1, 0))

num_nudge <- 0.29
num_size <- 5
clue_size <- 6.25

p <- df_ordered %>%
  ggplot(aes(column, row, label = value)) +
  geom_tile(fill = "white", color = "black", height = 1, width = 1, lwd = 1) +
  geom_tile(data = filter(df_ordered, valid == 0), fill = "black", color = "black", lwd = 1) +
  geom_text(color = "black", fontface = "bold", family = "Nanum Gothic", size = 10) +
  scale_y_reverse() +
  annotate("text", size = 14, label = "THE 20 MOST COMMON WORDS", x = 9.5, y = 13, color = "white", family = "Gentium Book Basic", fontface = "bold") +
  annotate("text", size = 12, label = "IN THE TIMES CROSSWORD PUZZLE",  x = 9.5, y = 13.6, color = "white", family = "Gentium Book Basic") +
  annotate("text", size = 7, label = "The clues below are samples from previous crosswords", fontface = "italic", x = 9.5, y = 14.3, color = "white", family = "Gentium Book Basic") +
  annotate("text", size = 6, label = "Graphic: Joshua Feldman", x = 9.5, y = 15, color = "white", family = "Gentium Book Basic") +
  annotate("text", label = "1", x = 5 - num_nudge, y = 1 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "2", x = 7 - num_nudge, y = 1 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "3", x = 3 - num_nudge, y = 2 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "4", x = 5 - num_nudge, y = 3 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "5", x = 1 - num_nudge, y = 5 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "6", x = 5 - num_nudge, y = 5 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "7", x = 7 - num_nudge, y = 6 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "8", x = 13 - num_nudge, y = 6 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "9", x = 5 - num_nudge, y = 7 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "10", x = 9 - num_nudge, y = 7 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "11", x = 11 - num_nudge, y = 7 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "12", x = 7 - num_nudge, y = 8 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "13", x = 4 - num_nudge, y = 9 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "14", x = 6 - num_nudge, y = 9 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "15", x = 3 - num_nudge, y = 11 - num_nudge, size = num_size, fontface = "bold") +
  annotate("text", label = "16", x = 4 - num_nudge, y = 11 - num_nudge, size = num_size, fontface = "bold") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 16.25, ymax = Inf, fill = "white", color = "white") +
  annotate("text", label = "Across", size = 9, x = 3.25, y = 16.75, color = "black", hjust = 0, family = "Gentium Book Basic") +
  annotate("text", label = "1. Draw connection (3)", x = 0, y = 17.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "3. I might be carried off when picked up (3)", x = 0, y = 18, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "4. One who'll succeed in audition for show (3)", x = 0, y = 18.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "5. Put pen to paper, did you say? That’s correct (5)", x = 0, y = 19, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "9. Public not allowed (3)", x = 0, y = 19.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "10. Friend, male one in past (5)", x = 0, y = 20, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "12. The man dismissed from Ashes is a fool (3)", x = 0, y = 20.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "13. Just passable (4)", x = 0, y = 21, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "15. Healthy source of water (4)", x = 0, y = 21.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "Down", size = 9, x = 3.25 * 3, y = 16.75, color = "black", hjust = 0, family = "Gentium Book Basic") +
  annotate("text", label = "1. Light meal Athena regularly provides (3)", x = 3.25 * 3 - 3.25, y = 17.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "2. Failing to start, expensive sound processing unit (3)", x = 3.25 * 3 - 3.25, y = 18, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "3. What's in some gap that's found in alphabet  (5)", x = 3.25 * 3 - 3.25, y = 18.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "5. Possibly an extra sequence (3)", x = 3.25 * 3 - 3.25, y = 19, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "6. Pair of books about keeping whiskey (3)", x = 3.25 * 3 - 3.25, y = 19.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "7. Jump, ultimately failing to take the lead (4)", x = 3.25 * 3 - 3.25, y = 20, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "8. Most superior upper garment (3)", x = 3.25 * 3 - 3.25, y = 20.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "10. Cry of pleasure about small tree (3)", x = 3.25 * 3 - 3.25, y = 21, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "11. Reserve needed by Celtic eleven (3)", x = 3.25 * 3 - 3.25, y = 21.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "14. Medicine initially mislaid, suffering (3)", x = 3.25 * 3 - 3.25, y = 22, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  annotate("text", label = "16. More old paintings put up (5)", x = 3.25 * 3 - 3.25, y = 22.5, color = "black", hjust = 0, size = clue_size, family = "Gentium Book Basic") +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_line(color = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white", family = "Nanum Gothic"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_equal()

ggsave("graphic.png")
