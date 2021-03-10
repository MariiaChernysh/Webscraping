# This code allows to receive Google Trends data
# The output consists of 5 datasets
library(gtrendsR)
library(reshape2)
library(dplyr)

#insert constants
word="Biden"
loc="US"
h24="now 1-d"
types<-c("web", "news", "images", "froogle", "youtube")

#Dynamics of search frequency for a certain word
google.trends = gtrends(word, gprop = types, time = "now 1-d")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL

#Extract datasets 
#create empty datasets
tr_region<-NULL
tr_time<-NULL
tr_country<-NULL
tr_dma<-NULL
tr_city<-NULL
#Loop over grooping types to bind them into datasets
for (t in types){
  tr_region <- rbind(tr_region,(gtrends(word, geo = loc, gprop = t, time = h24))$interest_by_region)
  tr_time<-rbind(tr_time,(gtrends(word, geo = loc, gprop = t, time = h24))$interest_over_time)
  tr_country<-rbind(tr_country,(gtrends(word, gprop = t, time = h24))$interest_by_country)
  tr_dma<-rbind(tr_dma,(gtrends(word, geo = loc, gprop = t, time = h24))$interest_by_dma)
  tr_city<-rbind(tr_city,(gtrends(word, geo = loc, gprop = t, time = h24))$interest_by_city)
}

data<-group_by(google.trends, gprop)
pivot<-summarize(data,
                 hits=sum(hits))

#https://www.displayr.com/extracting-google-trends-data-in-r/
#https://awesome-r.com/
