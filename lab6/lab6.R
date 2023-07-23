library(gutenbergr)
library(dplyr)
library(magrittr)
library(tidytext)
library(tidyr)
library(ggplot2)

g <- gutenberg_works()

id <- c(83, 103, 164, 1268)
verne <- gutenberg_download(id)

books <- g[g$gutenberg_id %in% id, c("gutenberg_id","title")]

verne %<>% left_join(books) %>%
  mutate(gutenberg_id = NULL)

verne_books <- verne %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc <- get_sentiments("nrc")

table(nrc$sentiment)

pos_neg <- verne_books %>%
  inner_join(nrc) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  count(word, sort = T) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

ggplot(pos_neg) + geom_col(aes(word, n, fill = sentiment)) + 
  coord_flip() + 
  facet_wrap( ~ sentiment, scales = "free")


nrc_class <- verne_books %>%
  filter(title == books[["title"]][2]) %>%
  inner_join(nrc) %>%
  filter(!(sentiment %in% c("positive", "negative"))) %>%
  group_by(sentiment) %>%
  count(word, sort = T) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
ggplot(nrc_class) + geom_col(aes(word, n, fill = sentiment), show.legend = FALSE) + 
  coord_flip() + 
  facet_wrap(~sentiment, nrow = 3, scales = "free")

verne_senti_bing <- verne_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 80, sentiment) 
verne_senti_bing %<>%
  spread(sentiment, n, fill = 0) 

verne_senti_bing %<>%
  mutate(sentiment = positive - negative)

ggplot(verne_senti_bing, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ title, ncol = 2, scales = "free_x")
emo <- as_tibble(read.csv("http://www.fizyka.pw.edu.pl/~julas/TEXT/lab/Ratings_Warriner_et_al.csv", stringsAsFactors = F))
emo
library(reshape2)
library(wordcloud)
verne_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"),
                   max.words = 100)
