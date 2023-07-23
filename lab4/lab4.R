library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)
library(ggplot2)
g <- gutenberg_works()

v <- gutenberg_download(c(83, 103, 1268))
books <- g[g$gutenberg_id %in% c(83, 103, 1268),c("gutenberg_id","title")]

books

v %<>% left_join(books) %>%
  mutate(gutenberg_id = NULL)
v
verne_words <- v %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

total_words <- verne_words %>%
  group_by(title) %>%
  summarise(total = sum(n))

verne_words %<>% left_join(total_words)

verne_freq <- verne_words %>%
  group_by(title) %>%
  mutate(rank = row_number(), freq = n / total)

verne_freq
ggplot(verne_freq) + 
  geom_line(aes(x = rank, y = freq, color = title)) + 
  scale_x_log10() + scale_y_log10() +
  theme(legend.position = "bottom")
ggplot(verne_freq) + 
  geom_histogram(aes(x = freq, fill = title, color = title), alpha = 0.3, position="identity") + 
  scale_x_log10() + scale_y_log10()

library(fields)
data <- verne_freq %>% ungroup() %>% 
  filter(title == "The Mysterious Island") %>% 
  select(rank, freq)

log.scale <- function(x, n) exp(seq(log(x[1]), log(x[length(x)]), length.out = n))

ranks.scale <- with(data, seq(min(rank), max(rank), length.out = 20))
ranks.scale <- log.scale(ranks.scale, 20)
ranks.scale

freq.sb <- stats.bin(data$rank, data$freq, breaks = ranks.scale)
freq.sb
zipf1.lm <- lm(log(freq.sb$stats[2,]) ~ log(freq.sb$centers))
summary(zipf1.lm)


A <- zipf1.lm$coefficients[1]
alfa <- zipf1.lm$coefficients[2]

plot(freq.sb$centers, freq.sb$stats[2,], log="xy", xlab = "ranga r", 
     ylab = "czestoc f", pch = 19, cex = 1.2, 
     main = substitute(paste("Rangowe prawo Zipfa ", alpha,"=",a),list(a = round(alfa, 2))))

lines(ranks.scale, exp(A) * ranks.scale ** alfa, col = "red", lty = 2, lwd = 2)

freqs.scale <- with(data, seq(min(freq), max(freq), length.out = 20))
freqs.scale <- log.scale(freqs.scale, 20)

h <- hist(data$freq, breaks = freqs.scale, plot = FALSE)

freq.lm <- lm(log(h$density) ~ log(h$mids))
summary(freq.lm)

A <- freq.lm$coefficients[1]
alfa <- freq.lm$coefficients[2]

plot(h$mids, h$density, log="xy", xlab = "czestosc f", ylab = "P(f)", 
     pch = 19, cex = 1.2, 
     main = substitute(paste("Czestosciowe prawo Zipfa ", alpha,"=",a),list(a = round(alfa, 2))))

lines(freqs.scale, exp(A) * freqs.scale ** alfa, col = "red", lty = 2, lwd = 2)
verne_heaps <- v %>% 
  unnest_tokens(word, text) %>% 
  group_by(title) %>% 
  mutate(M = row_number(), V = cumsum(!duplicated(word)))

verne_heaps
verne_sum <- verne_heaps %>% 
  summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], b = lm(log10(V) ~ log10(M))$coefficients[2])

verne_sum
ggplot(verne_heaps, aes(M, V, color = title)) + 
  geom_line() + 
  geom_abline(data = verne_sum, aes(intercept = a, slope = b, color = title), linetype = "dashed") +
  theme(legend.position = "bottom") +
  scale_x_log10() + scale_y_log10() +
  facet_grid(~title)
make.lm <- function(data, threshold) {
  data %>%
    filter(M > 10 & M < threshold) %>%
    summarise(th = threshold, A = lm(log(V) ~ log(M))$coefficients[1], beta = lm(log(V) ~ log(M))$coefficients[2])
}

threshold <- seq(100, 20000, 100)

lm.res <- do.call(rbind, lapply(threshold, make.lm, data = verne_heaps))
lm.res

ggplot(lm.res) +
  geom_line(aes(th, beta, color = title)) +
  theme(legend.position = "bottom") +
  facet_grid(~title)

# policzyc wykladnik heapa (beta) dla 10 ksiazek w wersji EN i FR 
# (+ dla chetnych jego odchylenie standardowe)
# zrobic wykres na osi X wsp. beta dla wersji EN, na osi Y beta w wersji FR i 
# policzyc korelacje miedzy tymi wartosciami (funkcja cor.test)
# sprawdzic czy wykladniki beta beda takie same kiedy przetasujemy slowa: sample(words)