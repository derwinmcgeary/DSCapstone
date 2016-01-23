# Plan: create "Stupid Backoff" predictor
# Do this by getting ngrams up to 3, then search the last two words entered and return the most likely,
# if there are none, search the last one word entered and return the next one, if none, return most
# likely word (which is lame)

# When Stupid Backoff is functional, try contextually-aware selector: select which Stupid Backoff model
# by analysing which corpus the user is most likely employing. Do so by finding a selection of
# 1- or 2-grams which are characteristic of either tweets, blogs, or news. Then return the appropriate
# word from that model (or re-weight models and return a word from the meta-model)

# The create*grams functions should be once on the big server then the data should be loaded
# I might make them caching so that they work either way

library(data.table)
library(stringi)

load("Tweetmodeltable.RData")
load("Newsmodeltable.RData")
load("Blogmodeltable.RData")

nxtwrd <- function(patt,dtmodel) {
  # this should be a much faster function, as it uses a data table model
  patt <- sanitise(patt)
  answer <- data.table()
  for(lvl in length(dtmodel):1) {
    if(nrow(answer) < 1 ) {
      answer <- dtmodel[[lvl]][start == lastword(patt, lvl-1)]
    }
  }
  answer[1]$end
}

censor <- function(rude) {
  ragluv <- c("kcuf","gnikcuf","dekcuf",
              "rekcuf","skcuf","tnuc", "srekcuf",
              "stnuc","rekcufrehtom", "srekcufrehtom",
              "tihs", "etihs", "reggin", 
              "ikap", "cips", "gow", 
              "hctib", "sehctib", "toggaf", 
              "stoggaf", "gaf", "sgaf", 
              "ekik", "daehgar", "knihc", "erohw")
  
  for(xxx in stri_reverse(ragluv)) raw <- gsub(xxx,"",raw)
}
sanitise <- function(raw) {
  raw <- tolower(raw)
  raw <- gsub("[^'[:^punct:]]", "", raw, perl=T)
  raw <- gsub("[[:digit:]]","", raw, perl=T)
  raw <- gsub("[^[:print:]]","", raw, perl=T)
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
  out <- paste(patt[max(1,(length(patt)-n+1)):length(patt)], sep=" ", collapse=" ")
  out
}

identify <- function(patt) {
  
}

babble <- function(startphrase, dtmodel) {
  for(i in 1:15) {
    startphrase <- paste(startphrase, nxtwrd(startphrase,dtmodel), sep=" ")
  }
  startphrase
}
