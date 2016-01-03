library(tm)
library(stringi)

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

sanitise <- function(raw) {
  ragluv <- c("kcuf","gnikcuf","dekcuf",
              "rekcuf","skcuf","tnuc", "srekcuf",
              "stnuc","rekcufrehtom", "srekcufrehtom",
              "tihs", "etihs", "reggin", 
              "ikap", "cips", "gow", 
              "hctib", "sehctib", "toggaf", 
              "stoggaf", "gaf", "sgaf", 
              "ekik", "daehgar", "sknihc")
  
  raw <- tolower(raw)
  raw <- gsub("[^'[:^punct:]]", "", raw, perl=T)
  raw <- gsub("[[:digit:]]","", raw, perl=T)
  raw <- gsub("[^[:print:]]","", raw, perl=T)
  for(xxx in stri_reverse(ragluv)) raw <- gsub(xxx,"",raw)
  raw <- gsub("\\s+"," ",raw)
  raw
}

createmodel <- function(intext, n=3) {
  print("creating model")
    ugs <- ngframe(intext)
    mymodel <- list(ugs)
    for(i in 2:n) {
      gf <- ngframe(intext,i)
      ngs <- gf[-1]
      for(x in 1:i) { 
        ngs[(x+1)] <- stri_extract_all_words(gf$ng, simplify=T)[,x]
      }
      ngs <- subset(ngs,ngs$freq > 1)
      mymodel[[i]] <- ngs
    }
    mymodel
}

lastword <- function(patt, n=1) {
  patt <- unlist(strsplit(patt, split=" "))
  out <- paste(patt[(length(patt)-n+1):length(patt)], sep=" ", collapse=" ")
  out
}

nthlastword <- function(patt,n=1) {
  patt <- unlist(strsplit(patt, split=" "))
  patt[(length(patt) - n + 1)]
}

predixxor <- function(patt) {
  patt <- inpoot()
  patt <- sanitise(patt)
  answers <- rbind(tweetbigrams[grep(paste("^",lastword(patt)," ",sep=""),tweetbigrams$ng),][1,], 
        tweettrigrams[grep(paste("^",lastword(patt,2)," ",sep=""),tweettrigrams$ng),][1,])
  answers
}

nxtwrd <- function(patt,rnk=1) {
  patt <- sanitise(patt)
  answer <- subset(tqg, tqg$first == nthlastword(patt,3))
  answer <- subset(answer, answer$second == nthlastword(patt,2))
  answer <- subset(answer, answer$third == nthlastword(patt))
  if(nrow(answer) < 1) {answer <- subset(ttg, ttg$first == nthlastword(patt,2))
  answer <- subset(answer, answer$second == nthlastword(patt))}
  if(nrow(answer) < 1) answer <- subset(tbg, tbg$first == nthlastword(patt))
  if(nrow(answer) < 1) answer$last <- "the"
  answer$last[rnk]
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

tweets <- readLines("final/en_US/en_US.twitter.txt", skipNul = TRUE)
tweets <- sanitise(tweets)
ntm <- createmodel(tweets,4)
print("Size of tweet model")
format(object.size(ntm),unit="Mb")
save(ntm, file="Tweetmodel.RData")
rm(tweets)
rm(ntm)

news <- readLines("final/en_US/en_US.news.txt", skipNul = TRUE)
news <- sanitise(news)
nnm <- createmodel(news,4)
print("Size of news model")
format(object.size(nnm),unit="Mb")
save(nnm, file="Newsmodel.RData")
rm(news)
rm(nnm)

blogs <- readLines("final/en_US/en_US.blogs.txt", skipNul = TRUE)
blogs <- sanitise(blogs)
nbm <- createmodel(blogs,4)
print("Size of blog model")
format(object.size(nbm),unit="Mb")
save(nbm, file="Blogmodel.RData")
rm(blogs)
rm(nbm)

print("All models complete.")

# TODO: Tweet or news machine learning, crossvalidation, skipgrams for middle pronouns, 
#       part of speech for bigrams, get it onto the server, UX, Presentation