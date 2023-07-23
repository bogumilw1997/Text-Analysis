library(dplyr)
library(tidytext)
library(tm)
library(lsa)

d1 <- "Romeo and Juliet."
d2 <- "Juliet: O happy dagger!"
d3 <- "Romeo died by dagger."
d4 <- "'Live free or die', that's the New-Hampshire's motto."
d5 <- "Did you know, New-Hampshire is in New-England."

removeSpecialChars <- function(x) gsub("[^0-9A-Za-z///' ]"," ",x)

data <- tibble(doc_id = 1:5, text = c(d1, d2, d3, d4, d5))

corpus <- VCorpus(DataframeSource(as.data.frame(data)))

corpus <- corpus %>% tm_map(removeWords, stopwords("en")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

doc <- TermDocumentMatrix(corpus)

as.matrix(doc)

doc.lsa <- lsa(doc, 2)

doc.lsa

docs.xy <- t(diag(doc.lsa$sk) %*% t(doc.lsa$dk)) 
terms.xy <- doc.lsa$tk %*% diag(doc.lsa$sk) 

max.xy <- apply(rbind(apply(docs.xy, 2, max), apply(terms.xy, 2, max)), 2, max)
min.xy <- apply(rbind(apply(docs.xy, 2, min), apply(terms.xy, 2, min)), 2, min)

max.xy <- max.xy + 0.2 * abs(max.xy)
min.xy <- min.xy - 0.2 * abs(min.xy)

plot(docs.xy, pch = 19, col = "red", xlim = c(min.xy[1], max.xy[1]), ylim = c(min.xy[2], max.xy[2]))
text(docs.xy, col = "red", label = c("D1", "D2", "D3", "D4", "D5"), font = 2, pos = 3)
cosine(t(docs.xy)) 

arrows(0, 0, docs.xy[,1], docs.xy[,2], length = 0.1, col = "red")
points(terms.xy, pch = 19, cex = 0.8, col = rgb(0, 0, 1, 0.8))
text(terms.xy, label=rownames(terms.xy), cex = 0.85, col = rgb(0, 0, 1, 0.8), pos = 3)

query1 <- c("die", "dagger")
query1.xy <- apply(terms.xy[rownames(terms.xy) %in% query1,], 2, mean)


points(t(apply(terms.xy[rownames(terms.xy) %in% query1,], 2, mean)), col = "darkgreen", pch = 19)
text(t(apply(terms.xy[rownames(terms.xy) %in% query1,], 2, mean)), col = "darkgreen", label = "die, dagger", cex = 0.8, font = 2, pos = 3)

points(t(apply(terms.xy[rownames(terms.xy) %in% query1,], 2, mean)), col = "darkgreen", pch = 19)
text(t(apply(terms.xy[rownames(terms.xy) %in% query1,], 2, mean)), col = "darkgreen", label = "die, dagger", cex = 0.8, font = 2, pos = 3)

rbind(docs.xy, query1.xy) %>%
  t() %>%
  cosine() %>%
  round(3)

d1 <- "If you want to cause a commotion in any psychology department or any other place where animal and human behaviour is studied, all that you have to do is to claim that your dog loves you. Skeptics, critics, and even some ardent supporters will pour out into the halls to argue the pros and cons of that statement. Among the skeptics you will find the veterinarian Fred Metzge, of Pennsylvania State University, who claims that dogs probably don't feel love in the typical way humans do. Dogs make  investments in human beings because it works for them. They have something to gain from putting so-called emotions out there.  Metzger believes that dogs 'love' us only as long as we continue to reward their behaviours with treats and attention. For most dog owners, however, there is little doubt that dogs can truly love people."

d2 <- "Emotions guide our lives in a million ways. Whether we're inclined to hide and avoid or ponder and express them, most of us don't realize the extent to which they are driving our thoughts and behavior. Exploring our emotions is a worthy endeavor for anyone hoping to know and develop themselves, build healthy relationships, and pursue what they want in life. Recent research has even suggested that emotional intelligence is more important than IQ, showing that it 'predicts over 54% of the variation in success' in relationships, health, and quality of life. Our emotions can offer us clues into who we are as well as how we've been affected by our history. Many of our actions are initiated by emotion, which leads to the natural question of what emotions are being surfaced and why."

d3 <- "Curiosity is part of human nature. One of the first questions children learn to ask is 'why?' As adults, we continue to wonder. Using empirical methods, psychologists apply that universal curiosity to collect and interpret research data to better understand and solve some of society's most challenging problems. It's difficult, if not impossible, to think of a facet of life where psychology is not involved. Psychologists employ the scientific method - stating the question, offering a theory and then constructing rigorous laboratory or field experiments to test the hypothesis. Psychologists apply the understanding gleaned through research to create evidence-based strategies that solve problems and improve lives."

d4 <- "Olga, a 22-year-old woman in Saratov, Russia took her dog and her baby son Vadim to a park and met up with friends. After a few drinks, Olga went home and left her baby behind! Luckily, her dog Lada was with the baby. Olga woke the next morning and realized the child was missing. She thought Vadim had been abducted, but her father went to the park and found the baby in his pram, with Lada still beside him. The rottweiler had stood guard over him all night long. Vadim was wet and hungry, but unharmed, 
and was placed in the care of his grandmother."

data <- tibble(doc_id = 1:4, text = c(d1, d2, d3, d4))

corpus <- VCorpus(DataframeSource(as.data.frame(data)))

corpus <- corpus %>% tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

doc <- TermDocumentMatrix(corpus)

doc.lsa <- lsa(doc, 2)

docs.xy <- t(diag(doc.lsa$sk) %*% t(doc.lsa$dk)) 
terms.xy <- doc.lsa$tk %*% diag(doc.lsa$sk) 

max.xy <- apply(rbind(apply(docs.xy, 2, max), apply(terms.xy, 2, max)), 2, max)
min.xy <- apply(rbind(apply(docs.xy, 2, min), apply(terms.xy, 2, min)), 2, min)

max.xy <- max.xy + 0.2 * abs(max.xy)
min.xy <- min.xy - 0.2 * abs(min.xy)

plot(docs.xy, pch = 19, col = "red", xlim = c(min.xy[1], max.xy[1]), ylim = c(min.xy[2], max.xy[2]))
text(docs.xy, col = "red", label = c("D1", "D2", "D3", "D4"), font = 2, pos = 3)
arrows(0, 0, docs.xy[,1], docs.xy[,2], length = 0.1, col = "red")

points(terms.xy, pch = 19, cex = 0.8, col = rgb(0, 0, 1, 0.8))

text(terms.xy, label=rownames(terms.xy), cex = 0.85, col = rgb(0, 0, 1, 0.8), pos = 3)


points(terms.xy, pch = 19, cex = 0.8, col = rgb(0, 0, 1, 0.8))
text(terms.xy, label=rownames(terms.xy), cex = 0.85, col = rgb(0, 0, 1, 0.8), pos = 3)
library(topicmodels)

doc <- DocumentTermMatrix(corpus)

doc.lda <- LDA(doc, k = 2)
terms(doc.lda, 10)
topics(doc.lda)

doc.lda <- LDA(doc, k = 3)
terms(doc.lda, 10)
topics(doc.lda)

t(posterior(doc.lda)$terms)[1:10,] %>% round(4)
posterior(doc.lda)$topics %>% round(4)
library(tidytext)
library(ggplot2)

data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + scale_x_reordered() + coord_flip()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
beta_spread %>% 
  arrange(log_ratio) %>% 
  mutate(term = reorder(term, log_ratio)) %>% 
  filter(abs(log_ratio) >20) %>% 
  ggplot() + 
  geom_col(aes(term, log_ratio, fill = as.factor(sign(log_ratio)))) + 
  coord_flip()
library(topicdoc)

topic_diagnostics(ap_lda, AssociatedPress)
  