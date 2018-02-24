# Please change the file system path to your workspace wherever it appears in the script.
# This script uses the datasets created by running the ScriptForCreatingSets.R script file.
# This program will plot the map and bar graphs in R studio. And create and save the word clouds to image files.
library(methods)
install.packages("jsonlite")
library(jsonlite)
install.packages("tibble")
library(tibble)
install.packages("stringr")
library(stringr)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("tm")
library(tm)
install.packages("qdap")
library(qdap)
install.packages("data.table")
library(data.table)
install.packages("RWeka")
library(RWeka)
install.packages("ggplot2")
library(ggplot2)        
install.packages("ggmap")
library(ggmap) 
install.packages("maps")
library(maps)           
install.packages("leaflet")
library(leaflet)        
install.packages("knitr")
library(knitr)
install.packages("SnowballC")
library("SnowballC")
install.packages("wordcloud")
library("wordcloud")
install.packages("RColorBrewer")
library("RColorBrewer")


#Map Plot for Locations of High Rated Businesses

business <- read.csv("business_restaurants.csv")

ratings <- business %>% group_by(stars) %>% count()
ggplot(data=ratings, aes(x=cut(stars,c(0,0.9,1.9,2.9,3.9,5)),
                         y=n,fill=cut(stars, c(0,0.9,1.9,2.9,3.9,5)))) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "YlOrRd") + 
  labs(title = "Distribution of Overall Restaurant Ratings", 
       y = "Count of Ratings", x = "Star Category", fill = "Star Category") +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("1.5","2.5","3.5","4.5")) 

#Plot locations of top rated restaurants on the map
top_cities <- business %>% filter(stars>=4) %>% group_by(city) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(10)
top_cities$longitude <- rep(0,length(top_cities$city))
top_cities$latitude <- rep(0,length(top_cities$city))
long_lat <- geocode(as.character(top_cities$city))
top_cities$longitude <- long_lat[,1]
top_cities$latitude <- long_lat[,2]

map1 <- leaflet(top_cities) %>% addTiles() %>% setView(lng = -96.503906, lat = 38.68551, zoom = 4) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, weight = 0, radius=~n/100+10, fillOpacity = 0.7 , color="Red" , popup = ~city) 
map1

#-Generate sets for useful/cool/funny reviews

review <- read.csv("review_train.csv")

reviewsCool <- review %>% filter( review$cool > review$funny & review$cool > review$useful) 
reviewsUseful <- review %>% filter( review$useful > review$cool & review$useful > review$funny) 
reviewsFunny <- review %>% filter( review$funny > review$cool & review$funny > review$useful) 

write.csv(reviewsCool,"cool_reviews.csv")
write.csv(reviewsUseful,"useful_reviews.csv")
write.csv(reviewsFunny,"funny_reviews.csv")

#Generate word clouds 

indexes = sample(1:nrow(review), size=0.5*nrow(review))
review=review[indexes,]

reviewsLowRated <- review %>% filter(stars.x<2) 
reviewsHighRated <- review %>% filter(stars.x>4) 

review_textLow <- reviewsLowRated$text
review_textHigh <- reviewsHighRated$text

reviewSourceLow <- VectorSource(review_textLow)
reviewCorpusLow <- VCorpus(reviewSourceLow)
reviewSourceHigh <- VectorSource(review_textHigh)
reviewCorpusHigh <- VCorpus(reviewSourceHigh)

# Convert the text to lower case
reviewCorpusLow <- tm_map(reviewCorpusLow, tolower)
reviewCorpusHigh <- tm_map(reviewCorpusHigh, tolower)
# Remove numbers
reviewCorpusLow <- tm_map(reviewCorpusLow, removeNumbers)
reviewCorpusHigh <- tm_map(reviewCorpusHigh, removeNumbers)
# Remove punctuations
reviewCorpusLow <- tm_map(reviewCorpusLow, removePunctuation)
reviewCorpusHigh <- tm_map(reviewCorpusHigh, removePunctuation)
# Remove english common stopwords
reviewCorpusLow <- tm_map(reviewCorpusLow, removeWords, stopwords("english"))
reviewCorpusHigh <- tm_map(reviewCorpusHigh, removeWords, stopwords("english"))
# Eliminate extra white spaces
reviewCorpusLow <- tm_map(reviewCorpusLow, stripWhitespace)
reviewCorpusHigh <- tm_map(reviewCorpusHigh, stripWhitespace)

# Text stemming (reduces words to their root form)

reviewCorpusLow <- tm_map(reviewCorpusLow, stemDocument)
reviewCorpusHigh <- tm_map(reviewCorpusHigh, stemDocument)

rm(review)
rm(reviewsHighRated)
rm(reviewsLowRated)
rm(indexes)
rm(review_textHigh)
rm(review_textLow)


reviewCorpusLow <- Corpus(VectorSource(reviewCorpusLow))
reviewCorpusHigh <- Corpus(VectorSource(reviewCorpusHigh))

rm(reviewSourceHigh)
rm(reviewSourceLow)

reviewDTMLow <- DocumentTermMatrix(reviewCorpusLow)
reviewSparseDTMLow<- removeSparseTerms(reviewDTMLow, 0.98)
reviewDTMHigh <- DocumentTermMatrix(reviewCorpusHigh)
reviewSparseDTMHigh<- removeSparseTerms(reviewDTMHigh, 0.98)


review_mLow <- as.matrix(reviewSparseDTMLow)
review_mHigh <- as.matrix(reviewSparseDTMHigh)


reviewTDMLow <- TermDocumentMatrix(reviewCorpusLow)
reviewSparseTDMLow<- removeSparseTerms(reviewTDMLow, 0.98)
reviewTDMHigh <- TermDocumentMatrix(reviewCorpusHigh)
reviewSparseTDMHigh<- removeSparseTerms(reviewTDMHigh, 0.98)

review_m2Low <- as.matrix(reviewTDMLow)
review_m2High <- as.matrix(reviewTDMHigh)

v <- sort(rowSums(review_m2Low),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


vv <- sort(rowSums(review_m2High),decreasing=TRUE)
dd <- data.frame(word = names(vv),freq=vv)


write.csv(d,"LowRatedWords.csv")
write.csv(dd,"HighRatedWords.csv")
# Generate the WordCloud

# Please change the file system path for your workspace
par(bg="yellow")
png(file="WordCloudLow.png",width=1000,height=700, bg="white")
wordcloud(d$word, d$freq, min.freq=1,max.freq = 1000,max.words=1000, col=terrain.colors(length(d$word),alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Highest frequency words in reviews (Low Rated Businesses)", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

# Please change the file system path for your workspace
par(bg="yellow")
png(file="WordCloudHigh.png",width=1000,height=700, bg="white")
wordcloud(dd$word, dd$freq, min.freq=1,max.freq = 1000,max.words=1000, col=terrain.colors(length(dd$word),alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Highest frequency words in reviews (High Rated Businesses)", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()


par(bg="lightyellow")
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="red", main ="Most frequent words from reviews of low rated businesses",
        ylab = "Word frequencies")

par(bg="lightyellow")
barplot(dd[1:10,]$freq, las = 2, names.arg = dd[1:10,]$word,
        col ="red", main ="Most frequent words from reviews of high rated businesses",
        ylab = "Word frequencies")

# Word cloud for cool

review <- read.csv("cool_reviews.csv")
review_textLow <- review$text
reviewSourceLow <- VectorSource(review_textLow)
reviewCorpusLow <- VCorpus(reviewSourceLow)
# Convert the text to lower case
reviewCorpusLow <- tm_map(reviewCorpusLow, tolower)
# Remove numbers
reviewCorpusLow <- tm_map(reviewCorpusLow, removeNumbers)
# Remove punctuations
reviewCorpusLow <- tm_map(reviewCorpusLow, removePunctuation)
# Remove english common stopwords
reviewCorpusLow <- tm_map(reviewCorpusLow, removeWords, stopwords("english"))
# Eliminate extra white spaces
reviewCorpusLow <- tm_map(reviewCorpusLow, stripWhitespace)
# Text stemming (reduces words to their root form)
reviewCorpusLow <- tm_map(reviewCorpusLow, stemDocument)
rm(review)
rm(review_textLow)
reviewCorpusLow <- Corpus(VectorSource(reviewCorpusLow))
rm(reviewSourceLow)
reviewDTMLow <- DocumentTermMatrix(reviewCorpusLow)
reviewSparseDTMLow<- removeSparseTerms(reviewDTMLow, 0.98)
review_mLow <- as.matrix(reviewSparseDTMLow)
reviewTDMLow <- TermDocumentMatrix(reviewCorpusLow)
reviewSparseTDMLow<- removeSparseTerms(reviewTDMLow, 0.98)
review_m2Low <- as.matrix(reviewTDMLow)
v <- sort(rowSums(review_m2Low),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
indexes = sample(1:nrow(d), size=0.9*nrow(d))
d=d[indexes,]

# Generate the WordCloud
par(bg="yellow")
png(file="WordCloudCool.png",width=1000,height=700, bg="white")
wordcloud(d$word, d$freq, min.freq=1,max.freq = 1000,max.words=500, col=terrain.colors(length(d$word),alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Highest frequency words in reviews voted cool)", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

par(bg="lightyellow")
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="red", main ="Most frequent words from reviews voted cool",
        ylab = "Word frequencies")

review <- read.csv("cool_reviews.csv")
rText <- review$text
rText.factor <- factor(rText)
rText <- as.character(rText.factor)
d<-get_nrc_sentiment(rText)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:10000]))
#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_new3<-td_new[9:10,]
#Visualisation
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments for cool reviews")
qplot(sentiment, data=td_new3, weight=count, geom="bar",fill=sentiment)+ggtitle("Positive & Negative count for cool reviews")
#sentiment analysis
bin_vector <- get_sentiment(rText, method="bing")
review$score = bin_vector
write.csv(review, "cool_reviews_with_score.csv")
sum(bin_vector)
mean(bin_vector)
summary(bin_vector)
#--------------

# Word cloud for funny

review <- read.csv("funny_reviews.csv")
review_textLow <- review$text
reviewSourceLow <- VectorSource(review_textLow)
reviewCorpusLow <- VCorpus(reviewSourceLow)
# Convert the text to lower case
reviewCorpusLow <- tm_map(reviewCorpusLow, tolower)
# Remove numbers
reviewCorpusLow <- tm_map(reviewCorpusLow, removeNumbers)
# Remove punctuations
reviewCorpusLow <- tm_map(reviewCorpusLow, removePunctuation)
# Remove english common stopwords
reviewCorpusLow <- tm_map(reviewCorpusLow, removeWords, stopwords("english"))
# Eliminate extra white spaces
reviewCorpusLow <- tm_map(reviewCorpusLow, stripWhitespace)
# Text stemming (reduces words to their root form)
reviewCorpusLow <- tm_map(reviewCorpusLow, stemDocument)
rm(review)
rm(reviewsLowRated)
rm(indexes)
rm(review_textLow)
reviewCorpusLow <- Corpus(VectorSource(reviewCorpusLow))
rm(reviewSourceLow)
reviewDTMLow <- DocumentTermMatrix(reviewCorpusLow)
reviewSparseDTMLow<- removeSparseTerms(reviewDTMLow, 0.98)
review_mLow <- as.matrix(reviewSparseDTMLow)
reviewTDMLow <- TermDocumentMatrix(reviewCorpusLow)
reviewSparseTDMLow<- removeSparseTerms(reviewTDMLow, 0.98)
review_m2Low <- as.matrix(reviewTDMLow)
v <- sort(rowSums(review_m2Low),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
indexes = sample(1:nrow(d), size=0.9*nrow(d))
d=d[indexes,]
head(d, 10)

# Generate the WordCloud
par(bg="yellow")
png(file="WordCloudFunny.png",width=1000,height=700, bg="white")
wordcloud(d$word, d$freq, min.freq=1,max.freq = 1000,max.words=500, col=terrain.colors(length(d$word),alpha=0.9), random.order=FALSE, rot.per=0.3 )
#title(main = "Highest frequency words in reviews voted cool)", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

par(bg="lightyellow")
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="red", main ="Most frequent words from reviews voted funny",
        ylab = "Word frequencies")
review <- read.csv("funny_reviews.csv")
rText <- review$text
rText.factor <- factor(rText)
rText <- as.character(rText.factor)
d<-get_nrc_sentiment(rText)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:10000]))
#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_new3<-td_new[9:10,]
#Visualisation
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments for funny reviews")
qplot(sentiment, data=td_new3, weight=count, geom="bar",fill=sentiment)+ggtitle("Positive & Negative count for funny reviews")
#sentiment analysis
bin_vector <- get_sentiment(rText, method="bing")
review$score = bin_vector
write.csv(review, "funny_reviews_with_score.csv")
sum(bin_vector)
mean(bin_vector)
summary(bin_vector)
#--------------
#word cloud for useful

review <- read.csv("useful_reviews.csv")
review_textLow <- review$text
reviewSourceLow <- VectorSource(review_textLow)
reviewCorpusLow <- VCorpus(reviewSourceLow)
# Convert the text to lower case
reviewCorpusLow <- tm_map(reviewCorpusLow, tolower)
# Remove numbers
reviewCorpusLow <- tm_map(reviewCorpusLow, removeNumbers)
# Remove punctuations
reviewCorpusLow <- tm_map(reviewCorpusLow, removePunctuation)
# Remove english common stopwords
reviewCorpusLow <- tm_map(reviewCorpusLow, removeWords, stopwords("english"))
# Eliminate extra white spaces
reviewCorpusLow <- tm_map(reviewCorpusLow, stripWhitespace)
# Text stemming (reduces words to their root form)
reviewCorpusLow <- tm_map(reviewCorpusLow, stemDocument)
rm(review)
rm(reviewsLowRated)
rm(indexes)
rm(review_textLow)
reviewCorpusLow <- Corpus(VectorSource(reviewCorpusLow))
rm(reviewSourceLow)
reviewDTMLow <- DocumentTermMatrix(reviewCorpusLow)
reviewSparseDTMLow<- removeSparseTerms(reviewDTMLow, 0.98)
review_mLow <- as.matrix(reviewSparseDTMLow)
reviewTDMLow <- TermDocumentMatrix(reviewCorpusLow)
reviewSparseTDMLow<- removeSparseTerms(reviewTDMLow, 0.98)
review_m2Low <- as.matrix(reviewTDMLow)
v <- sort(rowSums(review_m2Low),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
indexes = sample(1:nrow(d), size=0.9*nrow(d))
d=d[indexes,]

# Generate the WordCloud
par(bg="yellow")
png(file="WordCloudUseful.png",width=1000,height=700, bg="white")
wordcloud(d$word, d$freq, min.freq=1,max.freq = 1000,max.words=500, col=terrain.colors(length(d$word),alpha=0.9), random.order=FALSE, rot.per=0.3 )
#title(main = "Highest frequency words in reviews voted cool)", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

par(bg="lightyellow")
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="red", main ="Most frequent words from reviews voted useful",
        ylab = "Word frequencies")
review <- read.csv("useful_reviews.csv")
rText <- review$text
rText.factor <- factor(rText)
rText <- as.character(rText.factor)
d<-get_nrc_sentiment(rText)
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:10000]))
#Transformation and  cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
td_new3<-td_new[9:10,]
#Visualisation
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments for useful reviews")
qplot(sentiment, data=td_new3, weight=count, geom="bar",fill=sentiment)+ggtitle("Positive & Negative count for useful reviews")
#sentiment analysis
bin_vector <- get_sentiment(rText, method="bing")
review$score = bin_vector
write.csv(review, "useful_reviews_with_score.csv")
sum(bin_vector)
mean(bin_vector)
summary(bin_vector)
