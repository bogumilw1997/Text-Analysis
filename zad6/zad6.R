library(dplyr)
library(tidytext)
library(magrittr)
library(tm)
library(e1071)
library(caret)

rm(list = ls())

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))

data.bbc.random <- data.bbc
data.bbc.random$emo <- abs(data.bbc.random$emo)

data.bbc.random %<>% 
  arrange(desc(emo)) %>% 
  slice(-c(501:(n()-500)))

data.bbc.random %>% 
  group_by(emo) %>% 
  summarise(n = n())

data.bbc.random$text <- sapply(data.bbc.random$text, enc2utf8)
data.bbc.random$emo <- as.factor(data.bbc.random$emo)

data.bbc.random <- data.bbc.random[sample(1:nrow(data.bbc.random)),]

data.bbc.random$doc_id <- 1:nrow(data.bbc.random)

make.tdm.matrix <- function(df){
  source <- DataframeSource(as.data.frame(df))
  
  corpus <- VCorpus(source)
  
  corpus %<>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers)
  
  tdm <- DocumentTermMatrix(corpus)
  return(tdm)
}
tdm <- make.tdm.matrix(data.bbc.random)

slim.tdm.matrix <- function(tdm, df, row.thresh, col.thresh){
  tdm <- tdm[, apply(tdm, 2, sum) > row.thresh]
  tdm <- as.matrix(tdm)
  ind <- apply(tdm, 1, sum) > col.thresh
  tdm <- tdm[ind, ]
  class <- df$emo[ind]
  return(list(tdm, class))
}

tmp <- slim.tdm.matrix(tdm,data.bbc.random, 4, 1)

tdm <- tmp[[1]]
class <- tmp[[2]]
dim(tdm); length(class)

make.svm.model <- function(tdm, class){
  
  levels(class) <- c("obj", "sub")
  data <- cbind(as.data.frame(tdm), class.out = class)
  
  fit <- trainControl(method = "cv", number = 10)
  model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)
  
  return(model)
}

model.random <- make.svm.model(tdm, class)
confusionMatrix(model.random)

minus.df <- data.bbc[data.bbc$emo == -1,]
plus.df <- data.bbc[data.bbc$emo == 1,]
zero.df <- data.bbc[data.bbc$emo == 0,]

minus.ind <- sample(nrow(minus.df), 250)
minus.df <- minus.df[minus.ind,]

plus.ind <- sample(nrow(plus.df), 250)
plus.df <- plus.df[plus.ind,]

zero.ind <- sample(nrow(zero.df), 500)
zero.df <- zero.df[zero.ind,]

data.bbc.equal <- rbind(zero.df, minus.df, plus.df)
data.bbc.equal$emo <- abs(data.bbc.equal$emo)

data.bbc.equal %>% 
  group_by(emo) %>% 
  summarise(n = n())

data.bbc.equal$text <- sapply(data.bbc.equal$text, enc2utf8)
data.bbc.equal$emo <- as.factor(data.bbc.equal$emo)

data.bbc.equal <- data.bbc.equal[sample(1:nrow(data.bbc.equal)),]

data.bbc.equal$doc_id <- 1:nrow(data.bbc.equal)

tdm <- make.tdm.matrix(data.bbc.equal)
tmp <- slim.tdm.matrix(tdm,data.bbc.equal, 4, 1)

tdm <- tmp[[1]]
class <- tmp[[2]]
dim(tdm); length(class)

model.equal <- make.svm.model(tdm, class)
confusionMatrix(model.random)
confusionMatrix(model.equal)

res <- resamples(list(Random = model.random, Equal = model.equal))
bwplot(res)
