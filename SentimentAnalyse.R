# code in DMAR chapter

# This code was last run sucessfully on Nov 2013 in the this environment:
# R version 3.0.2 (2013-09-25)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# and these packages:

# [1] topicmodels_0.1-12 slam_0.1-30        pvclust_1.2-2      plyr_1.8           Snowball_0.0-10   
# [6] rJava_0.9-4        tm_0.5-9.1         Matrix_1.1-0       sna_2.3-1          igraph_0.6.6      
# [11] ggplot2_0.9.3.1    stringr_0.6.2

# get package with functions for interacting with Twitter.com
require(twitteR) 

# Declare Twitter API Credentials
source(file = "AuthKeys.R")

# Create Twitter Connection
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#Get last 1500 tweets 
bitcoindata <- searchTwitter('#bitcoin', n=1500) 
# convert to data frame
df <- do.call("rbind", lapply(bitcoindata, as.data.frame)) 
# get column names to see structure of the data
names(df) 
# look at the first three rows to check content
head(df,3) 


#Write the df to a csv for our reference
 
write.table(df, file = "Tweets.csv", na="", sep = ",",row.names = FALSE)
# see how many unique Twitter accounts in the sample
length(unique(df$screenName)) 


# Create a new column of random numbers in place of the usernames and redraw the plots
# find out how many random numbers we need
n <- length(unique(df$screenName))
# generate a vector of random number to replace the names, we'll get four digits just for convenience 
randuser <- round(runif(n, 1000, 9999),0)
# match up a random number to a username
screenName <- unique(df$screenName)
screenName <- sapply(screenName, as.character)
randuser <- cbind(randuser, screenName)
# Now merge the random numbers with the rest of the Twitter data, and match up the correct random numbers with multiple instances of the usernames...
rand.df  <-  merge(randuser, df, by="screenName")


# This is based on Jeffrey Breen's excellent tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/

# download sentiment word list from here: http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar un-rar and put somewhere logical on your computer
hu.liu.pos = scan('E:/My Documents/My Papers/conferences/SAA2010/opinion-lexicon-English/positive-words.txt', what = 'character',comment.char=';') #load +ve sentiment word list
hu.liu.neg = scan('E:/My Documents/My Papers/conferences/SAA2010/opinion-lexicon-English/negative-words.txt',what = 'character',comment.char= ';') #load -ve sentiment word list
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
require(plyr)
aaa.text <- df$text # get text of tweets
length(aaa.text) #check how many tweets, make sure it agrees with the original sample size
head(aaa.text, 5) #check content sample, see that it looks as expected, no weird characters, etc. 
aaa.scores <- score.sentiment(aaa.text,pos.words,neg.words,.progress='text') # get scores for the tweet text 
# create a histogram of sentiment scores
ggplot(aaa.scores, aes(x=score)) + 
  geom_histogram(binwidth=1) + 
  xlab("Sentiment score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
  opts(axis.title.y=theme_text(size = 14, angle=90, vjust = -0.25)) + 
  opts(plot.margin = unit(c(1,1,2,2), "lines")) 
aaa.pos <- subset(aaa.scores,aaa.scores$score >= 2) # get tweets with only very +ve scores
aaa.neg <- subset(aaa.scores,aaa.scores$score <= -2) # get tweets with only very -ve scores

# Now create subset based on tweets with certain words, such as the high frequency words identified in the text mining. eg. science
scien <- subset(aaa.scores, regexpr("scien", aaa.scores$text) > 0)   # extract tweets containing only 'scien'
# plot histogram for this token, 
ggplot(scien, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'scien'") + ylab("Frequency") + theme_bw()  + opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + opts(axis.title.y = theme_text(size = 14, angle = 90, vjust = -0.25)) + opts(plot.margin = unit(c(1,1,2,2), "lines")) 
# repeat this block with different high frequency words

a.tdm.sp <- removeSparseTerms(a.tdm, sparse=0.989)  # I found I had to iterate over this to ensure the tdm doesn't get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
a.tdm.sp.df <- as.data.frame(inspect(a.tdm.sp )) # convert document term matrix to data frame
nrow(a.tdm.sp.df) # check to see how many words we're left with after removing sparse terms
# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
a.tdm.sp.df.sc.t <- t(scale(a.tdm.sp.df))
require(pvclust)
fit <- pvclust(a.tdm.sp.df.sc.t, method.hclust = "average", method.dist = "correlation", nboot = 10) # this method may take a few hours the bootstraping, you can reduce the nboot value for a quicker result
plot(fit, cex = 1.5, cex.pv = 1.2, col.pv = c(1,0,0), main="", xlab="", sub="")  # draw the dendrogram

require(slam)
a.tdm.sp.t <- t(a.tdm.sp) # transpose document term matrix, necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(a.tdm.sp.t)) # check median...
term_tfidf <- tapply(a.tdm.sp.t$v/row_sums(a.tdm.sp.t)[a.tdm.sp.t$i], a.tdm.sp.t$j,mean) * log2(nDocs(a.tdm.sp.t)/col_sums(a.tdm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.tdm.sp.t.tdif <- a.tdm.sp.t[,term_tfidf>=1.0] # keep only those terms that are slightly less frequent that the median
a.tdm.sp.t.tdif <- a.tdm.sp.t[row_sums(a.tdm.sp.t) > 0, ]
summary(col_sums(a.tdm.sp.t.tdif)) # have a look

# Before going right into generating the topic model and analysing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log liklihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log liklihood value.

require(topicmodels)
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(a.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
best.model.logLik.df <- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(best.model.logLik)))
ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
  xlab("Number of topics") + 
  ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
  opts(axis.title.y=theme_text(size = 14, angle=90, vjust= -0.25)) + 
  opts(plot.margin = unit(c(1,1,2,2), "lines"))  

# ggsave(file = "model_LL_per_topic_number.pdf") # export the plot to a PDF file
# it's not easy to see exactly which topic number has the highest LL, so let's look at the data...
best.model.logLik.df.sort <- best.model.logLik.df[order(-best.model.logLik.df$LL), ] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
best.model.logLik.df.sort # have a look to see what's at the top of the list, the one with the highest score
ntop <- best.model.logLik.df.sort[1,]$topics


lda <- LDA(a.tdm.sp.t.tdif, ntop) # generate a LDA model the optimum number of topics
get_terms(lda, 5) # get keywords for each topic, just for a quick look
get_topics(lda, 5) # gets topic numbers per document
lda_topics<-get_topics(lda, 5) 
beta <- lda@beta # create object containing parameters of the word distribution for each topic
gamma <- lda@gamma # create object containing posterior topic distribution for each document
terms <- lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id <- t(apply(beta, 1, order)) # order the beta values
beta_ranked <- lapply(1:nrow(id),function(i)beta[i,id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic





