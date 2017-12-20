#Package for Twitter in R
require(twitteR) 
##################################
# Declare Twitter API Credentials
source(file = "AuthKeys.R")
#Good practice to keep API credentials in separate file
#Create Twitter Connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#Get last 1500 tweets with keyword #bitcoin
bitcoindata <- searchTwitter('#', n=1500) 
# convert to data frame
df <- do.call("rbind", lapply(bitcoindata, as.data.frame)) 
#Get column names to see how the structure looks
names(df) 
#Peak at the first three rows to see how it looks
head(df,3) 
#Write the df to a csv for our reference
write.table(df, file = "Tweets.csv", na="", sep = ",",row.names = FALSE)
###################################
#Before we run our tweets through the sentiment analyser function we need to do some data cleaning to remove unnecessary characters, words, elements in the tweet.
#Data-cleaning done using regex matching
require(plyr)
#Get text of tweets and load it as unclean.tweet
unclean.tweet <- df$text
#Clean the tweet data up. 
#Use gsub function to do this. 
#Remove & characters
clean_tweet = gsub("&amp", "", unclean.tweet)
#Remove Retweet notations
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
#Remove mentions
clean_tweet = gsub("@\\w+", "", clean_tweet)
#Remove punctuations (also takes care of emojis)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
#Remove digits
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
#Remove urls
clean_tweet = gsub("http\\w+", "", clean_tweet)
#Remove tabspaces
clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
#Remove whitespaces
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
#Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://[a-z,A-Z,0-9]*", " ")
#Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
#Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
#Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") 
#Remove \n characters
clean_tweet <- gsub("\\n*","",clean_tweet)
#Now we remove non-english words using this hack
temphld <- grep("clean_tweet", iconv(clean_tweet, "latin1", "ASCII", sub="clean_tweet"))
#Subset original vector of words to exclude words with non-ASCII char
clean_tweet <- clean_tweet[-temphld]
#Check to see if data cleaning has been done and unnecessary characters and elements have been removed 
head(clean_tweet)
#Load into text
bitcoindata.text <- clean_tweet
#Check how many tweets, make sure it agrees with the original sample size
length(bitcoindata.text)
#Check content sample, see that it looks as expected, no weird characters, etc. 
head(bitcoindata.text, 20)
####################################
#Here we build the sentiment analyser function
#This function part here is attributed to the tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ .Go check it out
#The positive and negative word lists used here were downloaded from http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar 
#Load the list of +ve words
hu.liu.pos = scan(file = 'positive-words.txt', what = 'character',comment.char=';')
#Load the list of -ve words
hu.liu.neg = scan(file = 'negative-words.txt',what = 'character',comment.char= ';') 
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
##########################################
#Generate scores for the tweet content
bitcoindata.scores <- score.sentiment(bitcoindata.text,pos.words,neg.words,.progress='text') 
#Create a histogram of sentiment scores
colors <-"lightsteelblue"
require(ggplot2)
#Plot graph to display sentiment of tweets
ggplot(bitcoindata.scores, aes(x=score)) + 
  geom_histogram(binwidth=1,fill=colors)+
  xlab("Sentiment Score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14, face="bold", color="Black")) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust = -0.25, face="bold", color="Black")) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines")) + theme(axis.text.x = element_text(color = c(rep("Red",2),rep("Green",2))))
##########################################
#Store high +ve score tweets
bitcoin.pos <- subset(bitcoindata.scores,bitcoindata.scores$score >= 2)
#Store high -ve score tweets
bitcoin.neg <- subset(bitcoindata.scores,bitcoindata.scores$score <= -2)
##########################################
#Extract tweets containing the word Bitcoin or BTC
btc <- subset(bitcoindata.scores, regexpr("Bitcoin|BTC|bitcoin|btc", bitcoindata.scores$text) > 0)  
# plot histogram for this token, 
ggplot(btc, aes(x = score)) + geom_histogram(binwidth = 1,fill="mistyrose3") + xlab("Sentiment score for the token 'Bitcoin'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines")) 
# repeat for word = ether
eth <- subset(bitcoindata.scores, regexpr("Ether|Ethereum|ETH|ether|ethereum", bitcoindata.scores$text) > 0)  
# plot histogram for this token, 
ggplot(eth, aes(x = score)) + geom_histogram(binwidth = 1,fill="mistyrose3") + xlab("Sentiment score for the token 'Ethereum'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines")) 
# repeat for word = litecoin|LTC
ltc <- subset(bitcoindata.scores, regexpr("Litecoin|LTC|litecoin|ltc", bitcoindata.scores$text) > 0)  
# plot histogram for this token, 
ggplot(ltc, aes(x = score)) + geom_histogram(binwidth = 1,fill="mistyrose3") + xlab("Sentiment score for the token 'Litecoin'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines")) 
