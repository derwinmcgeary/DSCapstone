require(caret)
require(rpart)
require(tm)
require(dplyr)
getfirst <- function(patt, n=2) {
  out <- unlist(strsplit(patt, split=" "))
  out <- paste(out[0:n], sep=" ", collapse=" ")
  out
}
tweets <- readLines("final/en_US/en_US.twitter.txt", n=100)
news <- readLines("final/en_US/en_US.news.txt", n=100)

tweets <- data.frame(text=tweets, type=as.character("Tweet"), stringsAsFactors = FALSE)
news <- data.frame(text=news, type=as.character("News"), stringsAsFactors = FALSE)

all <- rbind(tweets,news)
all$firstfour <- as.character(sapply(all$text, getfirst, n=4))
all <- all[,-1]

inTrain <- createDataPartition(y=all$type, p=0.75, list=F)

training <- all[inTrain,]
testing <- all[-inTrain,]



getfrequency <- function(patt, corp) {
  tabl <- corp[[length(strsplit(patt, split=" "))]]
  tabl[start==tolower(patt)]$freq
}

extractfeatures <- function(all) {
  all$hash <- as.numeric(grepl("#", all$firstfour))*2
  all$question <- as.numeric(grepl("\\?", all$firstfour))
  all$exclaim <- as.numeric(grepl("\\!", all$firstfour))
  all$rt <- as.numeric(grepl("\\brt", tolower(all$firstfour))) * 1
  all$follow <- as.numeric(grepl("follow", tolower(all$firstfour)))*1
  all$like <- as.numeric(grepl("like", tolower(all$firstfour)))
  all$i <- as.numeric(grepl("I[ ']", all$firstfour))
  all$thanks <- as.numeric(grepl("[Tt]hank", all$firstfour))
  all$you <- as.numeric(grepl("you", tolower(all$firstfour)))
  all$haha <- as.numeric(grepl("hah", tolower(all$firstfour)))
  all$ellipsis <- as.numeric(grepl("\\.\\.\\.", tolower(all$firstfour)))
  all$colon <- as.numeric(grepl("[\\;\\:]", tolower(all$firstfour)))
  all$nice <- as.numeric(grepl("nice", tolower(all$firstfour)))
  all$u <- as.numeric(grepl("\\bu\\b", tolower(all$firstfour)))
  all$ing <- as.numeric(grepl("^.*ing\\b", tolower(all$firstfour)))
  all$sum <- all$hash + all$rt + 
    all$haha + all$thank + 
    all$follow + all$exclaim + all$i + all$question + all$like +
    all$ellipsis + all$colon + all$nice + all$u + all$ing
  all$prediction <- ifelse(all$sum > 0, "Tweet","News")
  all
}

training <- extractfeatures(training)
mytree <- train(type ~ ., method="parRF", data=training[,-2])
# haha + rt + follow + exclaim + question + hash + you + thanks + i + 
predict(mytree, newdata=extractfeatures(testing)[,-(1:2)], type="raw") -> predicted
testing <- extractfeatures(testing)
table(actual=testing$type, pred=testing$prediction)
table(actual=testing$type, pred=predicted)
print("Accuracy")
print(paste("Dumb model",sum(testing$type==testing$prediction)/nrow(testing)))
print(paste("Smart model",sum(testing$type==predicted)/nrow(testing)))

# Now we follow
# http://www.castagnetto.com/20150103_spamham-sms-classification-using-caret-and-naive-bayes.html