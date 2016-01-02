library(tm)
library(knitr)
library(stringi)

ngframe <- function(texts, n=1) {
  w <- strsplit(texts, " ", fixed = TRUE)
  w <- unlist(w)
  grams <- vapply(ngrams(w, n), paste, "", collapse = " ")
  freex <- table(grams)
  freq <- as.vector(freex)
  ng <- as.vector(names(freex))
  ntable <- as.data.frame(cbind(ng,freq))
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

tweets <- readLines("final/en_US/en_US.twitter.txt", n=100000, skipNul = TRUE)
tweets <- sanitise(tweets)

createmodel <- function(intext, n=3) {
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
tweetunigrams <- ngframe(tweets)

tweetbigrams <- ngframe(tweets,2)
tbg <- tweetbigrams[-1]
tbg$first <- stri_extract_all_words(tweetbigrams$ng, simplify=T)[,1]
tbg$last <- stri_extract_all_words(tweetbigrams$ng, simplify=T)[,2]
print("total tweet bigrams")
format(object.size(tweetbigrams), units="Mb")
print("size of reduced table")
format(object.size(tbg), units="Mb")


tweettrigrams <- ngframe(tweets,3)
ttg <- tweettrigrams[-1]
ttg$first <- stri_extract_all_words(tweettrigrams$ng, simplify=T)[,1]
ttg$second <- stri_extract_all_words(tweettrigrams$ng, simplify=T)[,2]
ttg$last <- stri_extract_all_words(tweettrigrams$ng, simplify=T)[,3]
ttg <- subset(ttg, ttg$freq > 1)
rm(tweettrigrams)

tweetquadgrams <- ngframe(tweets,4)
tqg <- tweetquadgrams[-1]
tqg$first <- stri_extract_all_words(tweetquadgrams$ng, simplify=T)[,1]
tqg$second <- stri_extract_all_words(tweetquadgrams$ng, simplify=T)[,2]
tqg$third <- stri_extract_all_words(tweetquadgrams$ng, simplify=T)[,3]
tqg$last <- stri_extract_all_words(tweetquadgrams$ng, simplify=T)[,4]
tqg <- subset(tqg, tqg$freq > 1)
print("total quadgram size")
format(object.size(tweetquadgrams), units="Mb")
print("quadgram object size")
format(object.size(tqg), units="Mb")
rm(tweetquadgrams)

tweetmodel <- list(tweetunigrams, tbg, ttg, tqg)
ntm <- createmodel(tweets,4)

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

nextwrd <- function(patt,model, rnk=1) {
  patt <- sanitise(patt)
  strlen <- length(strsplit(patt,split=" ")[[1]])
  depth <- length(model)
  answer <- character(0)
  for(i in depth:1) {
    if(length(answer)==0) {
      for(z in 1:i) {
      cutdown <- subset(model[[i]], model[[i]][] == nthlastword(patt,3))
      }
    }
  }
}

babble <- function(startphrase) {
  for(i in 1:10) {
    startphrase <- paste(startphrase, nxtwrd(startphrase), sep=" ")
  }
  startphrase
}
inpoot <- function() {
  patt <- readline("Enter words: ")
  patt
}

# TODO: Tweet or news machine learning, crossvalidation, skipgrams for middle pronouns, 
#       part of speech for bigrams, get it onto the server, UX, Presentation