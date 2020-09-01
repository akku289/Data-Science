library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
aurl <- "https://www.amazon.com/PlayStation-4-Slim-1TB-Console/dp/B071CV8CG2/ref=sr_1_4?dchild=1&fst=as%3Aoff&qid=1598839006&rnid=16225016011&s=videogames-intl-ship&sr=1-4#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"Playstation_4.txt",row.names = F)

Playstation_4 <- read.delim('Playstation_4.txt')
str(Playstation_4)
View(Playstation_4)
corpus <- Playstation_4[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(cleanset[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(cleanset[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(cleanset[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('Playstation','PS4','can'))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
w <- rowSums(tdm) 
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))

w <- sort(rowSums(tdm), decreasing = TRUE) 
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
