library(gutenbergr)
library(gridExtra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidytext)

rm(list = ls())

gutenberg_works() %>% colnames()

gutenberg_works(languages = "en") %>% 
  filter(author == "Joyce, James")

gutenberg_works(languages = "en") %>% 
  filter(author == "Verne, Jules")

ulysses <- gutenberg_download(4300)
twenty_th_leagues <- gutenberg_download(164)

ulysses_words <- ulysses %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  transmute(word = reorder(word, n), n)

twenty_th_leagues_words <- twenty_th_leagues %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  transmute(word = reorder(word, n), n)

g1 <- ulysses_words %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip() + ggtitle("Ulysses")

g2 <- twenty_th_leagues_words %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip() + ggtitle("20 thousand leagues under the sea")

g3 <- ulysses_words %>%
  anti_join(stop_words) %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()

g4 <- twenty_th_leagues_words %>%
  anti_join(stop_words) %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()

g <- grid.arrange(g1,g2,g3,g4, ncol=2, nrow=2, left = "with/without stop words")
