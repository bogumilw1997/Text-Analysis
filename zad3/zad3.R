library(gutenbergr)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(magrittr)

rm(list = ls())

g <- gutenberg_works()

verne_en <- gutenberg_works(languages = "en") %>% 
  filter(author == "Verne, Jules")

verne_fr <- gutenberg_works(languages = "fr") %>% 
  filter(author == "Verne, Jules")

verne_fr_ids <- c(799, 800, 4548, 4717, 4791, 5081, 5126, 7442, 11927, 14163)
verne_fr_books <- verne_fr[verne_fr$gutenberg_id %in% verne_fr_ids,c("gutenberg_id","title")]

verne_en_ids <- c(83, 103, 3526, 16457, 3748, 1355, 3808, 1842, 29413, 2083)
verne_en_books <- verne_en[verne_en$gutenberg_id %in% verne_en_ids,c("gutenberg_id","title")]

verne_fr_books <-verne_fr_books[match(verne_fr_ids, verne_fr_books$gutenberg_id),]
verne_en_books <-verne_en_books[match(verne_en_ids, verne_en_books$gutenberg_id),]

v <- gutenberg_download(c(verne_en_ids,verne_fr_ids))
v <- left_join(v, rbind(verne_en_books, verne_fr_books))

verne_heaps <- v %>% 
  unnest_tokens(word, text) %>% 
  group_by(title) %>% 
  mutate(M = row_number(), V = cumsum(!duplicated(word)))

verne_sum <- verne_heaps %>% 
  summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], a_error = summary(lm(log10(V) ~ log10(M)))$coefficients[1,2], 
            b = lm(log10(V) ~ log10(M))$coefficients[2], b_error = summary(lm(log10(V) ~ log10(M)))$coefficients[2,2])

verne_sum <- left_join(verne_sum, rbind(verne_en_books, verne_fr_books))

b_en <- verne_sum[match(verne_en_ids, verne_sum$gutenberg_id),]$b
b_fr <- verne_sum[match(verne_fr_ids, verne_sum$gutenberg_id),]$b

b_en_error <- verne_sum[match(verne_en_ids, verne_sum$gutenberg_id),]$b_error
b_fr_error <- verne_sum[match(verne_fr_ids, verne_sum$gutenberg_id),]$b_error

df <- data.frame(b_en, b_en_error, b_fr, b_fr_error)

g1 <- ggplot(df, aes(x=b_en, y=b_fr)) + 
  geom_pointrange(aes(ymin=b_fr-b_fr_error, ymax=b_fr+b_fr_error, 
                      xmin=b_en-b_en_error, xmax=b_en+b_en_error)) + 
  ggtitle(paste("Non-shuffle, cor:", round(cor.test(df$b_en, df$b_fr)$estimate, 2), 
                ", p-value:", round(cor.test(df$b_en, df$b_fr)$p.value, 3)))

v1 <- v %>% 
  unnest_tokens(word, text)%>% 
  group_by(title)

for (id in verne_fr_ids) {
  v1[v1$gutenberg_id == id, "word"] = sample(v1[v1$gutenberg_id == id, ]$word)
}

for (id in verne_en_ids) {
  v1[v1$gutenberg_id == id, "word"] = sample(v1[v1$gutenberg_id == id, ]$word)
}

verne_heaps <- v1 %>% 
  mutate(M = row_number(), V = cumsum(!duplicated(word)))

verne_sum <- verne_heaps %>% 
  summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], a_error = summary(lm(log10(V) ~ log10(M)))$coefficients[1,2], 
            b = lm(log10(V) ~ log10(M))$coefficients[2], b_error = summary(lm(log10(V) ~ log10(M)))$coefficients[2,2])

verne_sum <- left_join(verne_sum, rbind(verne_en_books, verne_fr_books))

b_en <- verne_sum[match(verne_en_ids, verne_sum$gutenberg_id),]$b
b_fr <- verne_sum[match(verne_fr_ids, verne_sum$gutenberg_id),]$b

b_en_error <- verne_sum[match(verne_en_ids, verne_sum$gutenberg_id),]$b_error
b_fr_error <- verne_sum[match(verne_fr_ids, verne_sum$gutenberg_id),]$b_error

df <- data.frame(b_en, b_en_error, b_fr, b_fr_error)

g2 <- ggplot(df, aes(x=b_en, y=b_fr)) + 
  geom_pointrange(aes(ymin=b_fr-b_fr_error, ymax=b_fr+b_fr_error, 
                      xmin=b_en-b_en_error, xmax=b_en+b_en_error)) + 
  ggtitle(paste("Shuffle, cor:", round(cor.test(df$b_en, df$b_fr)$estimate, 2), 
                ", p-value:", round(cor.test(df$b_en, df$b_fr)$p.value, 3)))
g <- grid.arrange(g1,g2, ncol=1, nrow=2)
