# The aim of this program is to produce one or more objects of 60MB or
# less which can be used to predict the next word given two previous words

library("tm")
library("RWeka")
library("stringi")
massive <- readLines("final/en_US/en_US.news.txt", n=300000)
massive <- gsub("[^'[:^punct:]]", "", massive, perl=T)
massive <- gsub("[:digit:]","", massive, perl=T)
myCorpus <- Corpus(VectorSource(massive))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removeNumbers)
# sanitised for your protection...
ragluv <- c("kcuf","gnikcuf","dekcuf",
            "rekcuf","skcuf","tnuc", "srekcuf",
            "stnuc","rekcufrehtom", "srekcufrehtom",
            "tihs", "etihs", "reggin", 
            "ikap", "cips", "gow", 
            "hctib", "sehctib", "toggaf", 
            "stoggaf", "gaf", "gafs", 
            "ekik", "daehgar", "sknihc")
myCorpus <- tm_map(myCorpus, removeWords, stri_reverse(ragluv))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
print("OK")

gramTokenizer <- function(x,n){
  n <- 1
  if(n>1) {
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  } else {
    tokenizer <- function(x) WordTokenizer(x)
  }
  tokenizer
}

corpulize <- function(x,n) {
  tokee <- gramTokenizer(x,n)
  thetdm <- TermDocumentMatrix(x, control=list(tokenize=tokee))
  thetdm
}

options(mc.cores=1)
unis <- corpulize(myCorpus,1)
# dos <- corpulize(myCorpus,2)
# tres <- corpulize(myCorpus,3)

