
# Importing the necessary packages ----------------------------------------

library(pacman)
p_load(readxl, tm, ggplot2, wordcloud, dplyr, ModelMetrics, stringi, e1071, stringr, gmodels)

# Importing the data ------------------------------------------------------

setwd("E:Desktop/naive bayes")
coronaTrain <- read.csv(file.choose(new = TRUE), as.is = TRUE, fileEncoding = "UTF-8")
coronaTest <- read.csv(file.choose(new = TRUE), as.is = TRUE , fileEncoding = "UTF-8")
View(coronaTrain)

# Creating a corpus -------------------------------------------------------

corpus_train <- Corpus(VectorSource(coronaTrain$OriginalTweet))

# Looking at the content --------------------------------------------------

inspect(corpus_train[1:5])

# Cleaning the corpus -----------------------------------------------------

corpus <- tm_map(corpus_train, tolower) # putting all to lowercase
corpus <- tm_map(corpus, removeNumbers) # removing the numbers
corpus <- tm_map(corpus, removePunctuation) # remove the punctuation
corpus <- tm_map(corpus, removeWords, stopwords()) # removing the stop words
corpus <- tm_map(corpus, stripWhitespace) # remove the white spaces


# For the test data -------------------------------------------------------

corpus_test <- Corpus(VectorSource(coronaTest$OriginalTweet))

# Cleaning the test corpus  -----------------------------------------------

corpus_test <- tm_map(corpus_test, tolower) # putting all to lowercase
corpus_test <- tm_map(corpus_test, removeNumbers) # removing the numbers
corpus_test <- tm_map(corpus_test, removePunctuation) # remove the punctuation
corpus_test <- tm_map(corpus_test, removeWords, stopwords()) # removing the stop words
corpus_test <- tm_map(corpus_test, stripWhitespace) # remove the white spaces


# Performing Tokenization/ Splitting into individual terms ----------------

corona_dtm_train <- DocumentTermMatrix(corpus)
corona_dtm_test <- DocumentTermMatrix(corpus_test)

# Splitting the data sets -------------------------------------------------
# The data was already splitted

corona_train = coronaTrain
corona_test = coronaTest 

corona_dtm_train <- corona_dtm_train
corona_dtm_test <- corona_dtm_test

corpus_train <- corpus
corpus_test <- corpus_test

# wordcloud ---------------------------------------------------------------
# We want to see which words are common in the specific categories
# This is done as follows, Our dependent variable has more than one leve;

coronaTrain$Sentiment <- as.factor(coronaTrain$Sentiment)
unique(coronaTrain$Sentiment)

pal1 <- brewer.pal(9, "YlGn")
pal1 <- pal1[-c(1:4)]

# The words are plotted in deacreasing order, according to their frequencies

wordcloud(corpus[coronaTrain$Sentiment == "Neutral"], min.freq = 40, colors = pal1)
wordcloud(corpus[coronaTrain$Sentiment == "Positive"], min.freq = 40, colors = brewer.pal(9, "Reds")[-c(1:4)])
wordcloud(corpus[coronaTrain$Sentiment == "Negative"], min.freq = 40, colors = brewer.pal(9, "Blues")[-c(1:4)])
wordcloud(corpus[coronaTrain$Sentiment == "Extremely Negative"], min.freq = 40, colors = brewer.pal(9, "YlOrBr")[-c(1:4)])
wordcloud(corpus[coronaTrain$Sentiment == "Extremely Positive"], min.freq = 40, colors = brewer.pal(9, "GnBu")[-c(1:4)])


# Finding the most frequent words -----------------------------------------

findFreqTerms(corona_dtm_train, 100)

# Creating the dtm from the most frequent words ---------------------------

corona_dict <- findFreqTerms(corona_dtm_train, 5)

corona_dtm_train <- DocumentTermMatrix(corpus_train, list(dictionary = corona_dict))
corona_dtm_test <- DocumentTermMatrix(corpus_test, list(dictionary = corona_dict))

# Creating a function for factor ------------------------------------------

convert_counts <- function(x) 
{
  x <- ifelse(x > 0, 1, 0) # ifelse(test, yes, no)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

# Applying the factor function --------------------------------------------

corona_dtm_train <- apply(corona_dtm_train, MARGIN = 2, convert_counts)
corona_dtm_test <- apply(corona_dtm_test, MARGIN = 2, convert_counts)

# Training the model ------------------------------------------------------

model <- naiveBayes(corona_dtm_train, coronaTrain$Sentiment, laplace = 2)
sentiment_pred <- predict(model, newdata = corona_dtm_test)

# Assessing the Accuracy of the mode; -------------------------------------

CrossTable(
  sentiment_pred,
  coronaTest$Sentiment,
  dnn = c("Predicted", "Actual"),
  prop.chisq = F,
  prop.t = F
)


results <- data.frame(
  Tweet = coronaTest$OriginalTweet,
  Sentiment_pred = sentiment_pred,
  original_senti = coronaTest$Sentiment
)

View(results)



