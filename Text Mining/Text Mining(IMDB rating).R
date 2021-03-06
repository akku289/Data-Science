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
aurl <- "https://www.imdb.com/title/tt8946378/reviews?ref_=tt_ov_rt"     
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)

write.table(IMDB_reviews,"Knives_out.txt",row.names = F)


Knives_out <- read.delim('Knives_out.txt')
str(Knives_out)
corpus <- Knives_out[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]


w <- rowSums(tdm) 
w <- subset(w, w>= 50) 
barplot(w, las = 2, col = rainbow(50))

w <- sort(rowSums(tdm), decreasing = TRUE) 
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
IMDB_reviews <- read.delim('Knives_out.txt')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)
s <- get_nrc_sentiment(reviews)
head(s)
