library(tidyverse)
library(tidytext)
library(spacyr)
library(wordcloud)
library(ggraph)
library(igraph)
require(gridExtra)

source("theme.R")

spacy_initialize(model = "en_core_web_sm")

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
plot_spacy <- spacy_parse(df$plot)

names <- tolower(plot_spacy$token[str_detect(plot_spacy$entity, "PERSON")])

plot_spacy_reduced <- plot_spacy %>% 
  filter(token != "Directed") %>% 
  filter(pos %in% c("ADJ", "NOUN", "VERB")) %>% 
  mutate(pos = as.factor(pos))

levels(plot_spacy_reduced$pos) <- c("ADJECTIVES", "NOUNS", "VERBS")

plot1 <- plot_spacy_reduced %>% 
  count(pos, lemma) %>% 
  group_by(pos) %>% 
  top_n(20, n) %>% 
  ungroup() %>% 
  mutate(lemma = reorder_within(lemma, n, pos)) %>% 
  ggplot(aes(lemma, n, fill = pos)) +
  geom_col() +
  coord_flip() +
  labs(title = '"PARANORMAL ACTIVITY": THE LANGUAGE OF HORROR',
       subtitle = "The most common words used in the plot summaries of 3,328 horror films (2012-17). The top graph shows the most\ncommon unigrams (single words); the bottom graph shows the most common bigrams (pairs of words).",
       x = NULL,
       y = NULL) +
  facet_wrap(~pos, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_reordered() +
  theme(strip.background = element_blank()) +
  guides(fill = FALSE)


bigrams <- df %>% 
  unnest_tokens(bigram, plot, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word1 %in% names) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% names) %>% 
  count(word1, word2) %>% 
  filter(n > 7)

bigram_graph <- bigrams %>% 
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
cols <- brewer.pal(4, "Set2")

plot2 <- ggraph(bigram_graph, layout = "fr") +
  labs(x = NULL,
       y = NULL,
       caption = "Graphic: Joshua Feldman") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = cols[4], size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, family = "Raleway",
                 color = "white", size = 4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

lay <- rbind(1, 1, 1, 1, 2, 2, 2)

grid.arrange(plot1, plot2, layout_matrix = lay)
