library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(gutenbergr)
library(igraph)
library(ggraph)
library(stopwords)

rm(list = ls())

g <- gutenberg_works(languages = "pl")
v <- gutenberg_download(c(8119, 27081))
books <- g[g$gutenberg_id %in% c(8119, 27081),c("gutenberg_id","title")]

v %<>% left_join(books) %>%
  mutate(gutenberg_id = NULL)

df <- v %>% 
  group_by(title) %>% 
  summarise(text = paste(text, collapse = " ")) %>%
  corpus()

stopwordsPL <- readLines("polish.stopwords.txt", encoding = "UTF-8")

# tok <- tokens_remove(tokens(df, remove_punct = T), stopwordsPL, padding = TRUE)

df %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(stopwordsPL) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(n =2, concatenator = " ") %>%
  dfm() %>%
  textstat_frequency(n = 20, groups = title) %>%
  ggplot() + 
  geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2) + 
  scale_y_reordered()+ ylab("Bi-grams")

plot_graph <- function(df, title_nr, top) {
  mi.freq <- df %>% 
    corpus_subset(title == books$title[title_nr]) %>%
    tokens(remove_punct = T) %>%
    tokens_remove(stopwordsPL) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_ngrams(n =2, concatenator = " ") %>%
    dfm() %>%
    textstat_frequency()
  
  bigram.sep <- as_tibble(mi.freq) %>% 
    separate(feature, c("word1", "word2"), " ")
  
  graph.bi <- bigram.sep %>% 
    select(!(rank:group)) %>%
    top_n(top) %>%
    graph_from_data_frame()
  
  ggraph(graph.bi, layout = "nicely") + 
    geom_edge_link(aes(width = frequency), color = "darkblue") + 
    scale_edge_width_continuous(range = c(0.5,2)) +
    geom_node_point(size = 3, color = "orange") +
    geom_node_text(aes(label = name), size = 3, nudge_y = 0.5)
}

g1 <- plot_graph(df, 1, 100)
g2 <- plot_graph(df, 2, 40)