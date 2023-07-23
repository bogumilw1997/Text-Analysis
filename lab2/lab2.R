library(dplyr)
library(tidyverse)

mr.ted <- c("  Litwo! Ojczyzno moja! ty jesteś jak zdrowie;",    
            "Ile cię trzeba cenić, ten tylko się dowie",
            "Kto cię stracił. Dziś piękność twą w całéj ozdobie",
            "Widzę i opisuję, bo tęsknię po tobie.")

df <- tibble(lines = 1:length(mr.ted), text = mr.ted)
df
library(tidytext)

df.words <- df %>%
  unnest_tokens(word, text)
df.words
df.words %>%
  count(word, sort = TRUE)
library(ggplot2)

df.words %>%
  count(word, sort = TRUE) %>%
  mutate(word1 = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word1, n)) +
  coord_flip()

band_members
band_instruments

band_members %>% inner_join(band_instruments)
band_members %>% left_join(band_instruments)
band_members %>% full_join(band_instruments)
band_members %>% anti_join(band_instruments)
library(gutenbergr)
gutenberg_works()
gutenberg_works(languages = "pl") %>% 
  filter(author == "Mickiewicz, Adam")
gutenberg_download(31536)

# stop words w tidy text
# 10 albo 20 najczesciej wystepujacych
# zlepic do 1 rysunku z 4 panelami

stop_words

df <- data.frame(x=rnorm(1000))
df1 <- data.frame(x=rnorm(1000), y = runif(1000))
g1 <- ggplot(df)
library(gridExtra)
grid.arrange(g1,g2,g3,g4)