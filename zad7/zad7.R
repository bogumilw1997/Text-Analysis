library(dplyr)
library(tidytext)
library(tm)
library(lsa)
library(gutenbergr)
library(topicmodels)
library(ggplot2)
library(topicdoc)

rm(list = ls())

g <- gutenberg_works()

verne_en <- gutenberg_works(languages = "en") %>% 
  filter(author == "Verne, Jules")

austen_en <- gutenberg_works(languages = "en") %>% 
  filter(author == "Austen, Jane")

verne_en_ids <- c(83, 103, 3526)
verne_en_books <- verne_en[verne_en$gutenberg_id %in% verne_en_ids,c("gutenberg_id","title", "author")]

austen_en_ids <- c(105, 121, 141)
austen_en_books <- austen_en[austen_en$gutenberg_id %in% austen_en_ids,c("gutenberg_id","title", "author")]

v <- gutenberg_download(c(verne_en_ids,austen_en_ids))
v <- left_join(v, rbind(verne_en_books, austen_en_books))

df <- v %>% 
  group_by(title, author) %>% 
  summarise(text = paste(text, collapse = " ")) 

removeSpecialChars <- function(x) gsub("[^0-9A-Za-z///' ]"," ",x)

docs_ids <- seq(1:6)
df$doc_id <- docs_ids

titles <- df$title

corpus_verne <- VCorpus(DataframeSource(as.data.frame(df[df$author == "Verne, Jules",])))
corpus_austen <- VCorpus(DataframeSource(as.data.frame(df[df$author == "Austen, Jane",])))

corpus_verne <- corpus_verne %>% tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

corpus_austen <- corpus_austen %>% tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

corpus <- VCorpus(DataframeSource(as.data.frame(df)))

corpus <- corpus %>% tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

make.lda.graph <- function(corpus, top.themes, top.terms, title){
  
  doc <- DocumentTermMatrix(corpus)
  lda <- LDA(doc, k = top.themes, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  print(paste0("Koherencja między tematami ", title, ":"))
  print(round(topic_diagnostics(lda, doc)$topic_coherence, digits = 2))
  
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(top.terms, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  g <- top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) + ggtitle(title) +
    facet_wrap(~ topic, scales = "free") + scale_x_reordered() + coord_flip() 
    
  
  plot(g)
}

make.lda.graph(corpus_verne, 2, 10, "Verne")
make.lda.graph(corpus_austen, 2, 10, "Austen")
make.lda.graph(corpus, 2, 10, "Verne + Austen")
make.lda.graph(corpus, 3, 10, "Verne + Austen 3-themes")

doc <- DocumentTermMatrix(corpus_verne)
lda <- LDA(doc, k = 2, control = list(seed = 1234))
topic_diagnostics(lda, doc)$topic_coherence
      