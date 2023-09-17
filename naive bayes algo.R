## naive bayes algorithm
## sms spam filtering

# directory
setwd("E:/Documents/R-Studio Programms/naive bayes")

# loading file
sms_raw<-read.csv(file.choose(), as.is=T)
View(sms_raw)

str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

## loading required package
library(tm) # text mining

## creting sms corpus(collection of docs)
sms_corpus<-Corpus(VectorSource(sms_raw$text))

# tooking at the contents
inspect(sms_corpus[1:3]) # use vector indexing

## transformation using tm_mapping
## cleaning the corpus

corpus_clean<-tm_map(sms_corpus, tolower) # converts all to lower case
corpus_clean<-tm_map(corpus_clean, removeNumbers) # removes digits
corpus_clean<-tm_map(corpus_clean, removePunctuation) # removes punctuation
corpus_clean<-tm_map(corpus_clean, removeWords, stopwords()) # removes stopwords e.g: and, is...
corpus_clean<-tm_map(corpus_clean, stripWhitespace) # remove extra whitespaces

## performing tokenization, splitting strings into tokens(individual units)

sms_dtm<-DocumentTermMatrix(corpus_clean)
View(sms_dtm)

## splitting datasets into train and test data sets
sms_raw_train<-sms_raw[1:4180, ]
sms_raw_test<-sms_raw[4181:5574, ]

sms_dtm_train<-sms_dtm[1:4180, ]
sms_dtm_test<-sms_dtm[4181:5574, ]

sms_corpus_train<-corpus_clean[1:4180]
sms_corpus_test<-corpus_clean[4181:5574]

## wordcloud
library(wordcloud)

wordcloud(sms_corpus_train, random.order = F)

# creating subsets of spam and ham
spam <- subset(sms_raw_train, type == "spam")
ham<-subset(sms_raw_train, type=="ham")

## looking at common words in the spam and ham datasets
## e'll set min.freq to 40, meaning 40 most common words
wordcloud(spam$text, min.freq = 30, scale=c(3, 0.5)) # scale=font size
wordcloud(ham$text, min.freq = 30, scale=c(3, 0.5)) # scale=font size

## finding frwquent words
findFreqTerms(sms_dtm_train, 100) # the no. parameter
                                  #is the no. of times words appear

# creating a list of freq terms
sms_dict<-(findFreqTerms(sms_dtm_train, 5)) # couldnt find dictionary()

sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))

sms_test <- DocumentTermMatrix(sms_corpus_test,
                               list(dictionary = sms_dict))


## naive bayes mostly deals with factors, but our data
## involves counts of frequency of word appearance
# we'll create a function to convert counts to factors
convert_counts <- function(x) 
{
  x <- ifelse(x > 0, 1, 0) # ifelse(test, yes, no)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

## applying the convert_count on sms_train and test
## one can use for looop, but lets use apply
## in apply, MARGIN 1=row, MARGIN 2=column
sms_train<-apply(sms_train, MARGIN=2, convert_counts)
sms_test<-apply(sms_test, MARGIN=2, convert_counts)

## loading package with NB theory
library(e1071)

# we first build a classifier
# we then build a prediction

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
## sms_classifier is a classifier that we've trained

## predict() is used to make predictions
sms_test_pred <- predict(sms_classifier, sms_test)

## using corsstable() to see accuracy
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, 
           prop.chisq = F, prop.t = F, 
           dnn=c("Predicted", "Actual")) ##dnn changes dimension names(row, col)

## setting laplace value
sms_classifier2<-naiveBayes(sms_train, sms_raw_train$type, laplace = 2)

sms_test_pred2<-predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_raw_test$type, 
           prop.t = F, prop.chisq = F, dnn=c("Predicted", "Actual"))

