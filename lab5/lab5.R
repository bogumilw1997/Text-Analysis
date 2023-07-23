library(tidyverse)
library(tidytext)

data <- tibble(doc_id = 1:3, text = 
                 c("Once upon a time there was a king who wished to have a child.",
                   "Once upon a time, in a faraway land, there lived a beautiful princess.",
                   "In the olden days, when wishing was having, there used to live a King."))

data
library(quanteda)

df.data <- corpus(data)
summary(df.data)
df.data %>%
  tokens()
df.data %>%
  tokens(remove_punct = TRUE) %>%
  dfm()
dfm.data <- df.data %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem()

dfm.data
dfm.data %>%
  as.matrix() %>%
  t()
library(quanteda.textstats)

textstat_simil(dfm.data, method = "cosine")
?textstat_simil
textstat_simil(dfm.data)
cor(t(as.matrix(dfm.data)))
textstat_dist(dfm.data)
library(quanteda.textplots)

dfm.data %>% 
  textplot_wordcloud(min_count = 1)
dfm.tfidf.data <- df.data %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_wordstem() %>%
  dfm_tfidf(scheme_tf = "prop")

round(t(as.matrix(dfm.tfidf.data)), 4)
textstat_simil(dfm.tfidf.data, method = "cosine")
options(width = 250)

summary(data_corpus_inaugural)
color <- sapply(seq(0.2, 1, 0.05), function(x) adjustcolor("darkblue", alpha.f = x))

data_corpus_inaugural %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("english")) %>%
  dfm_wordstem() %>%
  textplot_wordcloud(color = color, min_count = 20)

corpus_subset(data_corpus_inaugural, 
              President %in% c("Obama", "Trump", "Biden")) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  dfm_group(groups = President) %>%
  textplot_wordcloud(min_count = 5, comparison = TRUE, color = c("blue", "darkgreen", "red"))
corpus_subset(data_corpus_inaugural, 
              President %in% c("Obama", "Trump", "Biden")) %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  topfeatures(groups = President)
corpus_subset(data_corpus_inaugural, 
              President == "Biden") %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm() %>%
  textplot_wordcloud()
corpus_subset(data_corpus_inaugural, President == "Biden") %>% 
  tokens() %>% 
  kwic(pattern = "america", window = 4)

corpus_subset(data_corpus_inaugural, Year > 1949) %>%
  tokens() %>%
  kwic(pattern = "america") %>%
  textplot_xray()
data_corpus_inaugural_subset <- 
  corpus_subset(data_corpus_inaugural, Year > 1981) %>% tokens()

g <- textplot_xray(
  kwic(data_corpus_inaugural_subset, pattern = "america*", valuetype = "glob"),
  kwic(data_corpus_inaugural_subset, pattern = "war"),
  kwic(data_corpus_inaugural_subset, pattern = "econom*", valuetype = "glob")
)

g + aes(color = keyword) + 
  scale_color_manual(values = c("blue", "red", "green")) +
  theme(legend.position = "none")
library(gutenbergr)

g <- gutenberg_works()
v <- gutenberg_download(c(83, 103, 1268))
books <- g[g$gutenberg_id %in% c(83, 103, 1268),c("gutenberg_id","title")]

v %<>% left_join(books) %>%
  mutate(gutenberg_id = NULL)

v
df <- v %>% 
  group_by(title) %>% 
  summarise(text = paste(text, collapse = " ")) %>%
  corpus()

summary(df)
dfm.df <- df %>% tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>% 
  dfm()

dfm.df %>% textstat_frequency(n = 10)
dfm.df %>% 
  textstat_frequency(n = 10, groups = title) %>%
  ggplot() + 
  geom_col(aes(x = frequency, y = fct_reorder(feature, -rank), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2)
dfm.df %>%
  textstat_frequency(n = 10, groups = title) %>%
  ggplot() + 
  geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2) + 
  scale_y_reordered()
df.dfm <- df %>% 
  corpus_subset(title == books$title[3]) %>%
  tokens(remove_punct = T) %>%
  tokens_ngrams(concatenator = " ") %>%
  dfm()

df.dfm
df.dfm %>%
  textstat_frequency(n = 20)
df.dfm <- df %>% 
  corpus_subset(title == books$title[3]) %>%
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(concatenator = " ") %>%
  dfm()

df.dfm %>%
  textstat_frequency(n = 20)
df %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(concatenator = " ") %>%
  dfm() %>%
  textstat_frequency(n = 10, groups = title) %>%
  ggplot() + 
  geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2) + 
  scale_y_reordered()
mi.freq <- df %>% 
  corpus_subset(title == books$title[3]) %>%
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(concatenator = " ") %>%
  dfm() %>%
  textstat_frequency()
df %>% 
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(n = 3, concatenator = " ") %>%
  dfm() %>%
  textstat_frequency(n = 10, groups = title) %>%
  ggplot() + 
  geom_col(aes(x = frequency, y = reorder_within(feature, frequency, group), fill = group), show.legend = F) + 
  facet_wrap(~group, scales = "free", nrow = 2) + 
  scale_y_reordered()

mi.freq <- df %>% 
  corpus_subset(title == books$title[3]) %>%
  tokens(remove_punct = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(concatenator = " ") %>%
  dfm() %>%
  textstat_frequency()

bigram.sep <- as_tibble(mi.freq) %>% 
  separate(feature, c("word1", "word2"), " ")

bigram.sep %>% 
  filter(word2 == "island") %>% print(n = 20)
library(igraph)
library(ggraph)

graph.bi <- bigram.sep %>% 
  filter(word2 == "island") %>%
  select(!(rank:group)) %>%
  graph_from_data_frame()
ggraph(graph.bi) + 
  geom_edge_link(color = "gray") + 
  geom_node_text(aes(label = name))
bi.cut.g <- graph.bi - E(graph.bi)[E(graph.bi)$frequency < 5]
bi.cut.g
bi.cut.g <- bi.cut.g - V(bi.cut.g)[degree(bi.cut.g) < 1]
bi.cut.g
ggraph(bi.cut.g, layout="kk") + 
  geom_edge_link(aes(width = frequency), color = "darkblue") + 
  scale_edge_width_continuous(range = c(0.5,4)) +
  geom_node_point(size = 8, color = "orange") +
  geom_node_text(aes(label = name), nudge_y = 0.5)
bigram.sep <- as_tibble(mi.freq) %>% 
  separate(feature, c("word1", "word2"), " ")

graph.bi <- bigram.sep %>% 
  select(!(rank:group)) %>%
  top_n(100) %>%
  graph_from_data_frame()
ggraph(graph.bi, layout = "nicely") + 
  geom_edge_link(aes(width = frequency), color = "darkblue") + 
  scale_edge_width_continuous(range = c(0.5,2)) +
  geom_node_point(size = 3, color = "orange") +
  geom_node_text(aes(label = name), size = 3, nudge_y = 0.5)
