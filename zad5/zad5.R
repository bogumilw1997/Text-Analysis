library(gutenbergr)
library(dplyr)
library(magrittr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(reshape2)
library(wordcloud)

rm(list = ls())

g <- gutenberg_works()

ind <- 16457

v <- gutenberg_download(ind)

books <- g[g$gutenberg_id %in% ind,c("gutenberg_id","title")]

verne <- v %>% left_join(books) %>%
  mutate(gutenberg_id = NULL)

bigrams <- verne %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams <- bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

nrc <- get_sentiments("nrc")

bigrams <- bigrams %>% 
  left_join(parts_of_speech, by = c("word2" = "word")) %>% 
  filter(pos == "Noun") %>%
  left_join(parts_of_speech, by = c("word1" = "word"), suffix = c("2", "1"))

top_nouns <- bigrams %>% count(word2) %>% top_n(3)
top_nouns <- top_nouns[order(-top_nouns$n),]

print(paste("Most used nouns:", top_nouns$word2[1],",", top_nouns$word2[2],",", top_nouns$word2[3]))

top_bigrams <- bigrams %>% right_join(top_nouns, by = c("word2" = "word2"))

pos <- top_bigrams %>% group_by(word2,pos1) %>% 
  summarise(pos_count=n(),.groups = 'drop')

pos2 <- pos %>% group_by(word2) %>% 
  summarise(total_count=sum(pos_count),.groups = 'drop')

pos <- pos %>% left_join(pos2, by = c("word2" = "word2")) %>%
  mutate(nn = pos_count / total_count)

pos <- pos[order(-pos$total_count, -pos$nn),]

ggplot(pos) + 
  geom_bar(aes(x = reorder(pos1, -nn), nn, fill = reorder(pos1, nn)), stat="identity") + 
  coord_flip() + theme(legend.position = "none") + 
  labs(y = "fraction", x = "POS") + facet_wrap(~word2)

draw_cloud <- function(top_bigrams, top){
  
  top_bigrams[top_bigrams$word2 == top_nouns$word2[top], ]  %>%
    inner_join(get_sentiments("bing"), by = c("word1" = "word")) %>%
    count(word1, sentiment, sort = TRUE) %>%
    acast(word1 ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)
  
  text(x=0, y=0, top_nouns$word2[top], cex = 2)
  
}

draw_cloud(top_bigrams, 1)
# draw_cloud(top_bigrams, 2) #<- dla "miles" nie ma żadnych słów z lewej strony w słowniku 
draw_cloud(top_bigrams, 3)

# chumra dla wszystkich 3 słów
top_bigrams %>%
  inner_join(get_sentiments("bing"), by = c("word1" = "word")) %>%
  count(word1, sentiment, sort = TRUE) %>%
  acast(word1 ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"), max.words = 100)

text(x=0, y=0, paste(top_nouns$word2[1],",", top_nouns$word2[2],",", top_nouns$word2[3]), cex = 0.5)

#wartości emocjonalne po lewej stronie
top_bigrams_sentiments <- top_bigrams %>%
  left_join(get_sentiments("bing"), by = c("word1" = "word"))