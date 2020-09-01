data <- read.csv(file.choose())
str(data)
colnames(data) <- c("type", "text")

#Converting the text to utf-8 format
data$text <- iconv(data$text, to = "utf-8")

#Type as factor
data$type <- factor(data$type)

summary(data)
table(data$type)
prop.table(table(data$type))
library(caret)
library(tm)
library(wordcloud)
library(e1071)
install.packages("MLmetrics") 
library(MLmetrics)
set.seed(123)
train_index <- createDataPartition(data$type,p=.75,list = FALSE,
                                   times = 1)
train <- data[train_index,]
test <- data[train_index,]
prop.table(table(train$type))*100
prop.table(table(test$type))*100
Corpus <- Corpus(VectorSource(train$text))
print(Corpus)
Corpus[[1]]$content
Corpus <- tm_map(Corpus,content_transformer(tolower))
Corpus <- tm_map(Corpus,removeNumbers)
Corpus <- tm_map(Corpus,removeWords,stopwords())
Corpus <- tm_map(Corpus,removePunctuation)
Corpus <- tm_map(Corpus,stripWhitespace)
Corpus[[1]]$content
par(mfrow=c(1,2))
wordcloud(Corpus[train$type == "ham"], min.freq = 40, random.order = FALSE)
wordcloud(Corpus[train$type == "spam"], min.freq = 40, random.order = FALSE)

sms_dtm <- DocumentTermMatrix(Corpus, control = list(global = c(2, Inf)))
print(sms_dtm)
inspect(sms_dtm[1:10, 5:13])
#find words that appears at least 5 times
sms_features <- findFreqTerms(sms_dtm, 5)
summary(sms_features)
head(sms_features)
sms_dtm_train <- DocumentTermMatrix(Corpus, list(global = c(2, Inf), dictionary = sms_features))
print(sms_dtm_train)
convert_count <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels= c(0,1), labels = c("No","Yes"))
  return(x)
}
sms_dtm_train <- apply(sms_dtm_train, MARGIN = 2, convert_count)
head(sms_dtm_train[,1:5])
sms_classifier <- naiveBayes(sms_dtm_train, train$type)
sms_classifier[[2]][1:5]

corpus <- Corpus(VectorSource(testData$text))
#1. normalize to lowercase (not a standard tm transformation)
corpus <- tm_map(Corpus, content_transformer(tolower))
#2. remove numbers
corpus <- tm_map(Corpus, removeNumbers)
#3. remove stopwords e.g. to, and, but, or (using predefined set of word in tm package)
corpus <- tm_map(Corpus, removeWords, stopwords())
#4. remove punctuation
corpus <- tm_map(Corpus, removePunctuation)
#5. normalize whitespaces
corpus <- tm_map(Corpus, stripWhitespace)
sms_dtm_test <- DocumentTermMatrix(corpus, list(global = c(2, Inf), dictionary = sms_features))
print(sms_dtm_test)
sms_dtm_test <- apply(sms_dtm_test, MARGIN = 2, convert_count)
sms_dtm_test[1:10, 5:12]
sms_test_pred <- predict(sms_classifier, sms_dtm_test)
table(test$type, sms_test_pred)
ConfusionMatrix(sms_test_pred, test$type)
Accuracy(sms_test_pred, test$type)
F1_Score(sms_test_pred, test$type)
