#Mining twitter with R: https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
#https://www.r-bloggers.com/r-text-mining-on-twitter-prayformh370-malaysia-airlines/ 
setwd('C:/Users/Schuler/Desktop/R Examples/Twitter R')

#clear workspace
rm(list = ls(all = TRUE))

install.packages("twitteR")
install.packages("ROAuth")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("RCurl")
install.packages("wordcloud")
install.packages("RColorBrewer")

# install useful packages
install.packages(c("ggplot2", "gridExtra", "igraph", "Matrix", "plyr", "pvclust", "RColorBrewer", "rJava", "slam", "sna", "SnowballC", "stringr", "tm", "topicmodels", "wordcloud"))
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(stringr)
library(gridExtra)
library(Matrix)


library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(RCurl)
library(wordcloud)
library(RColorBrewer)


# Set SSL certs globally

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
apiKey <-  "XXXXXXXXXXXXXXXX"
apiSecret <- "XXXXXXXXXXXXXXXXXXX"
twitCred <- OAuthFactory$new(
  consumerKey = apiKey, 
  consumerSecret = apiSecret,
  requestURL = reqURL,
  accessURL = accessURL, 
  authURL = authURL
)
twitCred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
)
3879936
access_token="XXXXXXXXXXXXXXXXXXX"
access_secret="XXXXXXXXXXXXXXXXXXXXX"
setup_twitter_oauth(apiKey, apiSecret, access_token, access_secret)

2
############################################################
# Chunk  - 2 - Twitter Scrape #criticalrole  
############################################################

#WOTCstaff.list <- searchTwitter('#WOTCstaff', n=1000, cainfo="cacert.pem")  
#WOTCstaff.df = twListToDF(WOTCstaff.list)  
#write.csv(WOTCstaff.df, file='C:/temp/WOTCstaffTweets.csv', row.names=F)

#DiceCameraAction.list <- searchTwitter('#DiceCameraAction', n=1000, cainfo="cacert.pem")  
#DiceCameraAction.df = twListToDF(DiceCameraAction.list)  
#write.csv(DiceCameraAction.df, file='C:/temp/DiceCameraActionTweets.csv', row.names=F)

DnD4.list <- searchTwitter('#criticalrole', n=3000)  
DnD4.df = twListToDF(DnD4.list)  
write.csv(DnD4.df, file='C:/Users/Schuler/Desktop/R Examples/Twitter R/DnDTweets4.csv', row.names=F)

DnD5.list <- searchTwitter('#Scanlan', n=200)  
DnD5.df = twListToDF(DnD5.list)  
write.csv(DnD5.df, file='C:/Users/Schuler/Desktop/R Examples/Twitter R/DnDTweets5.csv', row.names=F)


file <- "DnDTweets4.csv"
df <- DnD4.df

#To use the #scanlan
#file <- "DnDTweets5.csv"
#df <- DnD5.df


# first clean the twitter messages by removing odd characters
df$text <- sapply(df$text,function(row) iconv(row,to = 'UTF-8'))
df$text<- tolower(df$text)

######  -  -  -  -  -  -  - Sentiment Analysis -  -  -  -  -  -  -  -

RunBreenSentimentAnalysis = TRUE
if(RunBreenSentimentAnalysis == TRUE) {
  
  # Upload sentiment library
  
  hu.liu.pos=scan('C:/Users/Schuler/Desktop/R Examples/Twitter R/positive-words.txt',
                  what='character',comment.char=';') #load +ve sentiment word list
  hu.liu.neg=scan('C:/Users/Schuler/Desktop/R Examples/Twitter R/negative-words.txt',
                  what='character',comment.char=';') #load -ve sentiment word list
  pos.words=c(hu.liu.pos) # can add terms here e.g. c(hu.liu.pos, 'newterm', 'newterm2')
  neg.words=c(hu.liu.neg)
  
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    
    # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us    
    # we want a simple array ("a") of scores back, so we use "l" + "a" + "ply" = "laply":    
    
    scores = laply(sentences, function(sentence, pos.words, neg.words) 
    {
      word.list = str_split(sentence, '\\s+') # split into words. str_split is in the stringr package            
      words = unlist(word.list)  # sometimes a list() is one level of hierarchy too much       
      # compare our words to the dictionaries of positive & negative terms        
      pos.matches = match(words, pos.words)        
      neg.matches = match(words, neg.words)             
      # match() returns the position of the matched term or NA. we just want a TRUE/FALSE:        
      pos.matches = !is.na(pos.matches)        
      neg.matches = !is.na(neg.matches)         
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():        
      score = sum(pos.matches) - sum(neg.matches)        
      return(score)   
    }, pos.words, neg.words, .progress=.progress )     
    
    scores.df = data.frame(score=scores, text=sentences)    
    return(scores.df)
  }
  
  scores <- score.sentiment(df$text,pos.words, neg.words, .progress = 'text')
  hist(scores$score)
  ggplot(scores, aes(x=score)) + geom_histogram(binwidth=1) + xlab("Sentiment score") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y=element_text(size = 14, angle=90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "cm")) # plots nice histogram
  ggsave(file = "OverallSentimentHistogram1.pdf") # export the plot to a PDF file
  
  # Extract documents by sentiment category neutral, positive, negative, very positive, and very negative 
  
  scores.neutral<-subset(scores,scores$score==0) # get documents with only neutral scores
  scores.pos<-subset(scores,scores$score>=1) # get documents with only positive scores
  scores.neg<-subset(scores,scores$score<=-1) # get documents with only negative scores
  scores.verypos<-subset(scores,scores$score>=2) # get documents with only very positive scores
  scores.veryneg<-subset(scores,scores$score<=-2) # get documents with only very negative scores
  
  
  # Export data
  
  write.csv(scores,file = "SentimentScores1.csv")
  write.csv(scores.neutral,file = "SentimentScores_Neutral1.csv")
  write.csv(scores.pos,file = "SentimentScores_Positive1.csv")
  write.csv(scores.neg,file = "SentimentScores_Negative1.csv")
  write.csv(scores.verypos,file = "SentimentScores_VeryPositive1.csv")
  write.csv(scores.veryneg,file = "SentimentScores_VeryNegative1.csv")
  
}

