# Sentiment Analysis of Tweets Related to Cryptocurrencies

Sentiment Analyses of Twitter tweets related to cryptocurrencies
## Packages Required

TwitteR, plyr, stringr, ggplot2, downloadable from CRAN

### Prerequisites

R and R Studio

## Dataset Description

Data is aggregated from Twitter using the TwitteR search api call. 
Here we search for the term "#Cryptocurrency" and look for the last 1500 tweets.(Max. Free limit)
This is raw tweet data as provided by the returned Json file.

After aggregating this data, data cleaning is done to remove unnecessary characters, numbers, non-english words, twitter symbols like @ RT and # to get raw text of tweets.
Simple Regex matching is used to clean data.

## Sentiment Analyser Description

A simple analyser function that increments or decrements points to a sentence based on words matched to a positive or negative word list.

This follows the tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ 

The scores are then stored and used to generate graph plots based on sentiment score. 

## Summary

Histogram modeling using ggplot is done using the sentiment score on the overall dataset.
Then we do the same for the term Bitcoin alone.



Likewise for Ethereum


Likewise for Litecoin



## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* https://github.com/benmarwick/AAA2011-Tweets

