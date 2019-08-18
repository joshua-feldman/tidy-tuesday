library(tidyverse)

df <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

df_wide <- df %>% 
  spread(key = department, value = rd_budget)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- cor(df_wide[,5:18])
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
cormat_melted <- reshape2::melt(upper_tri, na.rm = TRUE)

cormat_melted %>% 
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(name = "Correlation") +
  labs(title = "High spending on defense research is associated with low budget for environmental protection",
       subtitle = "Correlation matrix of R&D budget per US agency/department (1976–2017)",
       caption = "Source: American Association for the Advancement of Science") +
  theme_minimal(base_size = 20) +
  theme(text = element_text(family = "Lato"),
        plot.title = element_text(face = "bold"),
        axis.title = element_blank()) 



