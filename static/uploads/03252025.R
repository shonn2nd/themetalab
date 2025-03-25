#單純貝氏分類器
#Naive Bayes

# load packages ------------------------------------------------------------
library(tidyverse)
library(readr)
library(psych)
library(class)
library(gmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(naivebayes)

# import -------------------------------------------------------------
sms_raw<-read.csv("Chapter 04/sms_spam.csv")

str(sms_raw)

glimpse(sms_raw)

# trans ----------------------------------------------------------

sms_raw<-sms_raw %>% 
  mutate(type = as.factor(type))

str(sms_raw$type)

table(sms_raw$type)

# corpus ---------------------------------------------------------
#our goal: tokenization 代碼化

#create a corpus (a collection of documents) 語料庫
sms_corpus<-VCorpus(VectorSource(sms_raw$text))

#view our corpus
sms_corpus

#view the first and second document of our corpus
inspect(sms_corpus[1:2])

#view the actual message text
as.character(sms_corpus[[1]])
as.character(sms_corpus[[2]])

lapply(sms_corpus[1:2], as.character) #lapply

#lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#remove stop words
#停用詞
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

#remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#stemming
#詞幹提取
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#remove additional whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#tokenization
#see picture
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm

#trainig and test datasets
sms_dtm_train <- sms_dtm[1:4169, ] #75%
sms_dtm_test  <- sms_dtm[4170:5559, ] #25%

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#word clouds
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

spam<-sms_raw %>% 
  filter(type == "spam")

ham<-sms_raw %>% 
  filter(type == "ham")

#Display only the top 40 most frequent words in the word cloud.
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Return only the words (terms) that appear in at least 5 documents in the training dataset.
findFreqTerms(sms_dtm_train, 5)

sms_freq_words<-findFreqTerms(sms_dtm_train, 5)

str(sms_freq_words)

#Take the training and test DTM and keep only the columns (words) that appeared in at least 5 documents.
sms_dtm_freq_train<-sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[, sms_freq_words]

#Convert the word frequencies (number of times each word appears in each SMS) into simple binary values — "Yes" or "No" — to indicate presence or absence of a word.
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

#Apply the function to the columns of your document-term matrices
#see picture
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# training a model on the data --------------------------------------------
#The function naive_bayes() builds a model that learns the probability of spam based on which words are present or absent.
sms_classifier <- naive_bayes(sms_train, sms_train_labels)

#see picture
sms_classifier

# evaluating model performance --------------------------------------------
#make predictions on new (test) data
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


# improving model performance ---------------------------------------------
#Laplace smoothing is a technique to avoid problems with zero probabilities.
#see picture
sms_classifier2 <- naive_bayes(sms_train, sms_train_labels, laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
