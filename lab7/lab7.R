library(tidyverse)
library(tidytext)

parts_of_speech

parts_of_speech %>%
  count(word, sort = T)
pos <- parts_of_speech %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))

pos
ggplot(pos) + 
  geom_bar(aes(x = reorder(pos, -nn), nn, fill = reorder(pos, nn)), stat="identity") + 
  coord_flip() + theme(legend.position = "none") + 
  labs(y = "fraction", x = "POS")
library(gutenbergr)

verne <- gutenberg_download(83)

verne_book <- verne %>%
  unnest_tokens(word, text) %>%
  left_join(parts_of_speech) %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))

verne_book
ggplot(verne_book) + 
  geom_bar(aes(x = reorder(pos, -nn), nn, fill = reorder(pos, nn)), stat="identity") + 
  coord_flip() + theme(legend.position = "none") + 
  labs(y = "fraction", x = "POS")

verne_join <- verne_book %>% 
  full_join(pos, by = c("pos"))

verne_join
theme_set(theme_bw())

verne_join %>% 
  mutate(diff = log10(nn.x / nn.y)) %>% 
  ggplot() + geom_bar(aes(x = reorder(pos, diff), diff, fill = diff > 0), color = "grey", stat="identity") +
  scale_fill_manual(values = c("blue", "red")) + 
  coord_flip() + 
  theme(legend.position = "none") +
  labs(title="Cos", x = "POS", y = expression(paste(log[10]," ",bgroup("(",frac(n[Verne],n[POS]),")"))))

g <- gutenberg_works()

ind <- c(16457, 18857, 1268, 2488, 2083)

v <- gutenberg_download(ind)

books <- g[g$gutenberg_id %in% ind,c("gutenberg_id","title")]

verne <- v %>% left_join(books) %>%
  mutate(gutenberg_id = NULL)
verne_bigrams <- verne %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_sep <- verne_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_flt <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_flt
bigrams_flt %>% 
  filter(word2 == "land") %>% 
  left_join(parts_of_speech, by = c("word1" = "word")) %>% 
  filter(pos == "Adjective") %>%
  count(title, word1) %>%
  group_by(title) %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word1 = reorder(word1, n), n) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(word1, n), n), stat="identity") + 
  coord_flip() + 
  facet_wrap(~title)


nrc <- get_sentiments("nrc")

bigrams_flt %>% 
  filter(word2 == "land") %>% 
  left_join(parts_of_speech, by = c("word1" = "word")) %>% 
  filter(pos == "Adjective") %>%
  count(title, word1) %>%
  inner_join(nrc, by = c("word1" = "word")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(title) %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word1 = reorder(word1, n)) %>%
  ggplot() + 
  geom_bar(aes(x = word1, n, fill = sentiment), stat="identity") + 
  coord_flip() + 
  facet_wrap(~title)

rm(list = ls())

library(wordnet)

library(igraph)


setDict("./dict/")

verne <- gutenberg_download(83)


# Dodajemy parts of speech
verne_books <- verne %>%
  unnest_tokens(word, text) %>%
  left_join(parts_of_speech)

verne_books <- verne %>%
  unnest_tokens(word, text) %>%
  count(word, sort = T)


verne_books <-
  verne_books %>%
  left_join(parts_of_speech) %>%
  filter(pos == "Adjective")

N <- 400

syn <- lapply(verne_books$word[1:N], synonyms, pos = "ADJ")

from <- unlist(lapply(1:N, function(i) rep(verne_books$word[i], length(syn[[i]]))))
to <- unlist(syn)

syn1 <- lapply(to, synonyms, pos = "ADJ")
from1 <- unlist(lapply(1:length(to), function(i) rep(to[i], length(syn1[[i]]))))
to1 <- unlist(syn1)

df <- tibble(from = c(from, from1), to = c(to, to1))

df$from <- gsub("\\([ap]\\)", "", df$from)
df$to <- gsub("\\([ap]\\)", "", df$to)

df

g <- graph_from_data_frame(df, directed = F)
g <- simplify(g)


g.good <- make_ego_graph(g, order = 2, nodes = V(g)[V(g)$name == "good"])[[1]]

plot(g.good, vertex.size = 5, vertex.color = "#ffaabb55", vertex.label.dist = 0.75, vertex.label.cex = 0.75, vertex.label.color = "black")

g <- graph_from_data_frame(df, directed = F)
g <- simplify(g)


g.good <- make_ego_graph(g, order = 2, nodes = V(g)[V(g)$name == "good"])[[1]]

plot(g.good, vertex.size = 5, vertex.color = "#ffaabb55", vertex.label.dist = 0.75, vertex.label.cex = 0.75, vertex.label.color = "black")
