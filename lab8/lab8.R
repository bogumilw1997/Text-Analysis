library(dplyr)
library(tidytext)
library(magrittr)

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))

data.bbc
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n = n())
data.bbc %<>% 
  arrange(desc(emo)) %>% 
  slice(-c(501:(n()-498)))

data.bbc$text <- sapply(data.bbc$text, enc2utf8)
data.bbc$emo <- as.factor(data.bbc$emo)

data.bbc <- data.bbc[sample(1:nrow(data.bbc)),]

data.bbc$doc_id <- 1:nrow(data.bbc)
library(tm)
source <- DataframeSource(as.data.frame(data.bbc))

corpus <- VCorpus(source)

corpus %<>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)

tdm <- DocumentTermMatrix(corpus)

tdm
tdm.count <- apply(tdm, 2, sum)
library(MASS)

h <- hist(tdm.count, breaks = max(tdm.count), plot = F)

plot(h$mids, h$counts, log="xy", pch = 19, xlab = "ile razy słowo powtórzyło się w dokumentach", ylab="liczba przypadków")
abline(v = 1.5, col = "blue", lty = 2)

tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm

tdm <- as.matrix(tdm)

ind <- apply(tdm, 1, sum) > 1

tdm <- tdm[ind, ]
class <- data.bbc$emo[ind]

dim(tdm); length(class)

CM <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  return(sum(diag(CM)) / sum(CM))
}

bbc.lda <- lda(tdm, class)
bbc.lda.pred <- predict(bbc.lda, tdm)

table(class, bbc.lda.pred$class)
CM(class, bbc.lda.pred$class)

bbc.lda.pred <- predict(bbc.lda, tdm)

table(class, bbc.lda.pred$class)

library(e1071)

bbc.svml <- svm(tdm, class, type = "C-classification", kernel = "linear")
bbc.svml.pred <- predict(bbc.svml, tdm)

table(class, bbc.svml.pred)
CM(class, bbc.svml.pred)

bbc.svmr <- svm(tdm, class, type = "C-classification")
bbc.svmr.pred <- predict(bbc.svmr, tdm)

table(class, bbc.svmr.pred)
CM(class, bbc.svmr.pred)
library(caret)

levels(class) <- c("obj", "sub")
data <- cbind(as.data.frame(tdm), class.out = class)


fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)
model

model.lda <- train(class.out ~ ., data = data, method = "lda", trControl = fit)
res <- resamples(list(LDA = model.lda, SVM = model))
bwplot

confusionMatrix(model)
confusionMatrix(model.lda)

# Zad 6
# Porownac wyniki dla laczenia komentarzy w sposob losowy a kiedy wezniemy po rowno z grupy 1 i -1.
