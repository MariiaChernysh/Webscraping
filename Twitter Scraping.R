library(twitteR)
library(wordcloud)
library(tm)
library(RCurl)

consumer_key <- '§§§§§§§'
consumer_secret <- '§§§§§§§'
access_token <- '§§§§§§§'
access_key <- '§§§§§§§'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_key)
1
#list of locales - https://stackoverflow.com/questions/3191664/list-of-all-locales-and-their-short-codes
# max N of tweets - 3200
data <- searchTwitter("news+London+app", n=3200, locale= "en_GB ISO-8859-1")
datap <- twListToDF(data)
View(datap)
write.table(datap, 'BBC.csv', sep=';')


#To work with data
data <- read.csv('BBC.csv', sep=';')
pz_text <- data$text
str(pz_text)
#Clean
pz <- Corpus(VectorSource(pz_text))
inspect(pz)
pz <- tm_map(pz,content_transformer(tolower))
pz <- tm_map(pz, removePunctuation)
pz <- tm_map(pz, removeWords, stopwords("english"))
pz <- tm_map(pz, stripWhitespace)
pz <- tm_map(pz, removeNumbers)
pz <- tm_map(pz, removeWords, c("111",  "222", "333")) 

#NEW wordcloud
library(wordcloud)

col=brewer.pal(6,"Dark2")
wordcloud(pz, min.freq=5, scale=c(5,1),rot.per = 0.5,
          random.color=T, max.word=100, random.order=F,colors=col)
#OLD Wordcloud
wordcloud(pz, random.order = F, scale = c(10,0.5), max.words = 1000, min.freq = 5, colors = rainbow(118))

#Dendogram of most common words. Create matrix
pzdm <- TermDocumentMatrix(pz)
# Remove sparse terms         
findFreqTerms(pzdm, lowfreq = 30)
pzdm <- removeSparseTerms(pzdm, sparse = 0.99)
#scale it
pzdms <- scale(pzdm)
#calculate the distance for clustering
dist <- dist(pzdms, method = "euclidean")
#use hierarchical clustering
fit <- hclust(dist)
# Create Dendrogram
par(mai=c(1,0.5,1,0.1))
plot(fit, xlab="", sub="", col.main="salmon")
