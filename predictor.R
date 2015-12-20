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

create3grams <- function(corp) {
  
}

create2grams <- function(corp) {
  
}

create1grams <- function(corp) {
  
}

