library(tm)
library(stringi)

sanitise <- function(raw) {
  raw <- tolower(raw)
  raw <- gsub("[^'[:^punct:]]", "", raw, perl=T)
  raw <- gsub("[[:digit:]]","", raw, perl=T)
  raw <- gsub("[^[:print:]]","", raw, perl=T)
  raw <- gsub("\\s+"," ",raw)
  raw
}

ngframe <- function(texts, n=1) {
  print("ngram frame")
  w <- strsplit(texts, " ", fixed = TRUE)
  w <- unlist(w)
  grams <- vapply(ngrams(w, n), paste, "", collapse = " ")
  freex <- table(grams)
  freq <- as.vector(freex)
  ng <- as.vector(names(freex))
  ntable <- as.data.frame(cbind(ng,freq))
  rm(freex)
  rm(freq)
  rm(ng)
  ntable$freq <- as.numeric(as.character(ntable$freq))
  ntable$ng <- as.character(ntable$ng)
  ntable[order(ntable$freq, decreasing=TRUE),]
}

nthlastword <- function(patt,n=1) {
  patt <- unlist(strsplit(patt, split=" "))
  patt[max(1,(length(patt) - n + 1))]
}

predixxor <- function(patt) {
  patt <- inpoot()
  patt <- sanitise(patt)
  answers <- rbind(tweetbigrams[grep(paste("^",lastword(patt)," ",sep=""),tweetbigrams$ng),][1,], 
        tweettrigrams[grep(paste("^",lastword(patt,2)," ",sep=""),tweettrigrams$ng),][1,])
  answers
}


nextwrd <- function(patt,model) {
  # this is a terrible function, but I can't see a way to make it much nicer
  # the problem is the variable number of columns to match
  patt <- sanitise(patt)
  strlen <- length(strsplit(patt,split=" ")[[1]])
  depth <- length(model)
  searchdepth <- min(depth, strlen + 1)
  answer <- character(0)
  for(i in searchdepth:2) {
    if(length(answer)==0) {
      ngtable <- model[[i]]
      for(x in 1:(ncol(ngtable)-2)) {
        if(nrow(ngtable) > 0) ngtable <- subset(ngtable, ngtable[(x + 1)]==nthlastword(patt,i-x))
      }
      if(nrow(ngtable) > 0) answer <- ngtable[1,ncol(ngtable)]
    }
  }
  if(length(answer)==0) answer <- "the"
  answer
}

dumptable <- function(patt, model) {
  # This just gives the complete list of matches
  patt <- sanitise(patt)
  strlen <- length(strsplit(patt,split=" ")[[1]])
  depth <- length(model)
  searchdepth <- min(depth, strlen + 1)
  answer <- character(0)
  for(i in searchdepth:2) {
    if(length(answer)==0) {
      ngtable <- model[[i]]
      for(x in 1:(ncol(ngtable)-2)) {
        if(nrow(ngtable) > 0) ngtable <- subset(ngtable, ngtable[(x + 1)]==nthlastword(patt,i-x))
      }
      if(nrow(ngtable) > 0) answer <- ngtable
    }
  }
  if(length(answer)==0) answer <- "No matches"
  answer
}

babble <- function(startphrase, mymodel) {
  for(i in 1:15) {
    startphrase <- paste(startphrase, nextwrd(startphrase,mymodel), sep=" ")
  }
  startphrase
}
inpoot <- function() {
  patt <- readline("Enter words: ")
  patt
}

ngtodatatable <- function(ng) {
  # merge the middle columns
  if(ncol(ng) == 6) ng$start <- paste(ng[,2],ng[,3], ng[,4], ng[,5], sep=" ")
  if(ncol(ng) == 5) ng$start <- paste(ng[,2],ng[,3], ng[,4], sep=" ")
  if(ncol(ng) == 4) ng$start <- paste(ng[,2],ng[,3], sep=" ")
  if(ncol(ng) == 3) ng$start <- ng[,2]
  if(ncol(ng) == 2) {
    ng <- ng[c(2,1)]
    colnames(ng) <- c("freq", "start")
  }
  print("create data table")
  output <- data.table(freq = ng$freq,start = ng$start,end=ng[,ncol(ng)-1])
  print("set key")
  setkey(output,start)
  output <- output[order(-rank(freq),start)]
  print("done")
  output
}

dataframetodatatablemodel <- function(dfmodel) {
  output <- list()
  for(i in 1:length(dfmodel)) {
    output[[i]] <- ngtodatatable(dfmodel[[i]])
  }
  output
}

if(!file.exists("Tweetmodel.RData")){
tweets <- readLines("final/en_US/en_US.twitter.txt", skipNul = TRUE)
tweets <- sanitise(tweets)
ntm <- createmodel(tweets,4)
print("Size of tweet model")
print(format(object.size(ntm),unit="Mb"))
save(ntm, file="Tweetmodel.RData")
rm(tweets)
rm(ntm)
}

if(!file.exists("Newsmodel.RData")) {
news <- readLines("final/en_US/en_US.news.txt", skipNul = TRUE)
news <- sanitise(news)
nnm <- createmodel(news,4)
print("Size of news model")
print(format(object.size(nnm),unit="Mb"))
save(nnm, file="Newsmodel.RData")
rm(news)
rm(nnm)
}

if(!file.exists("Blogmodel.RData")) {
blogs <- readLines("final/en_US/en_US.blogs.txt", skipNul = TRUE)
blogs <- sanitise(blogs)
nbm <- createmodel(blogs,4)
print("Size of blog model")
print(format(object.size(nbm),unit="Mb"))
save(nbm, file="Blogmodel.RData")
rm(blogs)
rm(nbm)
}

print("All models complete.")
