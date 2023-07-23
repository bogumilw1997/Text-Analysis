library(dplyr)
library(tidytext)

data <- tibble(doc_id = 1:3, text = 
                 c("Once upon a time there was a king who wished to have a child.",
                   "Once upon a time, in a faraway land there lived a beautiful princess.",
                   "In the olden time, when wishing was having, there used to live a King."))

data
library(tm)

doc_words <- data %>%
  unnest_tokens(word, text) %>%
  count(doc_id, word, sort = TRUE)

doc_words

total_words <- doc_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

total_words
doc_words %>%
  mutate(tf = n / total)

df.source <- DataframeSource(as.data.frame(data))
corpus <- VCorpus(df.source)

inspect(corpus)
corpus[[2]]$meta
corpus[[2]]$content
corpus1 <- corpus %>%
  tm_map(removePunctuation)

content(corpus1[[3]])
corpus1 <- corpus %>%
  tm_map(content_transformer(tolower))

content(corpus1[[3]])
tdm <- TermDocumentMatrix(corpus)
tdm
inspect(tdm)

tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE))
tdm <- as.matrix(tdm)
tdm


tdm <- TermDocumentMatrix(corpus, control = list(
  removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE))
tdm <- as.matrix(tdm)
tdm
tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
tdm
as.matrix(tdm)
data("acq")
length(acq)
acq[[1]]$content
# przerabianie maciezy gorna polowka bez przekatnej
# 4 histogramy
# IF/ IF IDF na pionie, iloczyn skalarny/cosinusowy na pionie
# Wziac najwieksza wartosc dla kazdej maciezy
A[upper.tri(A)]
