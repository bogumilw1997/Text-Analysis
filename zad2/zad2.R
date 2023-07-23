library(tm)
library(dplyr)
library(tidytext)
library(gridExtra)
library(tidyverse)
library(ggplot2)

rm(list = ls())

findMaxIdx <- function(matrix){
  
  max_indx <- as.vector(which(matrix==max(matrix), arr.ind=T))
  
  return(max_indx)
}

get_TF_matrix <- function(corpus){
  
  tdm <- TermDocumentMatrix(corpus, control = list(
    removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE))
  tdm <- as.matrix(tdm)
  
  return(tdm)
}

get_TFIDF_matrix <- function(corpus){
  
  tdm <- TermDocumentMatrix(corpus, control = list(
    removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, weighting = weightTfIdf))
  tdm <- as.matrix(tdm)
  
  return(tdm)
}

make_cross_hist <- function(corpus, type){
  
  if(type == "TF"){
    tdm <- get_TF_matrix(corpus)
  } else {
    tdm <- get_TFIDF_matrix(corpus)
  }

  tdm <- crossprod(tdm)
  
  tdm_ <- tdm[upper.tri(tdm)]
  
  data <- as.data.frame(tdm_)
  
  max_sim <- max(tdm_)
  
  max_indx <- findMaxIdx(tdm - diag(max(tdm), nrow(tdm),ncol(tdm)))
  
  print(paste("Maximum", type, "cross similarity:", round(max_sim, 4), sep = " "))
  print(paste("For documents:", max_indx[1],"and" , max_indx[2], sep = " "))
  writeLines("\n")
  
  g <- ggplot(data, aes(x=tdm_)) + geom_histogram(color="black", fill="grey") + xlab("cross similarity") + 
    ggtitle(paste(type,"+ cross"))
  
  return(g)
}

make_cos_hist <- function(corpus, type){
  
  if(type == "TF"){
    tdm <- get_TF_matrix(corpus)
  } else {
    tdm <- get_TFIDF_matrix(corpus)
  }
  
  f2 <- function(x) sqrt(sum(x^2))
  
  tdm <- (crossprod(tdm) / (apply(tdm, 2, f2) * apply(tdm, 2, f2))) %>% round(6)
  
  tdm_ <- tdm[upper.tri(tdm)]
  
  data <- as.data.frame(tdm_)
  
  max_sim <- max(tdm_)
  
  max_indx <- findMaxIdx(tdm - diag(max(tdm), nrow(tdm),ncol(tdm)))
  
  print(paste("Maximum", type, "cosinus similarity:", round(max_sim, 4), sep = " "))
  print(paste("For documents:", max_indx[1],"and" , max_indx[2], sep = " "))
  writeLines("\n")
  g <- ggplot(data, aes(x=tdm_)) + geom_histogram(color="black", fill="grey") + xlab("cosinus similarity") + 
    ggtitle(paste(type,"+ cosinus"))
  
  return(g)
}

data("acq")

g1 <- make_cross_hist(acq, "TF")
g2 <- make_cross_hist(acq, "TF-IDF")
g3 <- make_cos_hist(acq, "TF")
g4 <- make_cos_hist(acq, "TF-IDF")

g <- grid.arrange(g1,g3,g2,g4, ncol=2, nrow=2, left = "TF-IDF/TF", top = "cross/cosinus")
