# TODO: Add comment
# 
# Author: mario
###############################################################################

#To run all examples of this demo, please set the folling variables with
#according Application-ID values:
#nytimes_appid,  bing_appid

library(tm.plugin.webmining)
#### retrieve corpus from Google Finance News for Microsoft stock
test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))

#inspect first 10 elements of retrieved corpus
inspect(test1corp[1:10])

#check meta data of first element in the corpus
meta(test1corp[[1]])


#### retrieve corpus from Twitter Search API for the search Term 'Microsoft'
test2corp <- Corpus(TwitterSource("Microsoft"))

#inspect first 3 elements of retrieved corpus
inspect(test2corp[1:3])

#check meta data of first element in the corpus
meta(test1corp[[1]])


#### retrieve corpus from Twitter Search API for the search Term 'Microsoft'
test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))

#inspect first 3 elements of retrieved corpus
inspect(test2corp[1:3])

#check meta data of first element in the corpus
meta(test1corp[[1]])







#test ok
test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))

#test ok
test4corp <- Corpus(YahooInplaySource())

#test ok
test5corp <- Corpus(YahooFinanceSource("MSFT"))

#test ok
test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))

test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))

#test ok
test8corp <- Corpus(YahooNewsSource("Microsoft"))

#test ok
test9corp <- Corpus(GoogleNewsSource("Microsoft"))

#test ok
test10corp <- Corpus(ReutersNewsSource("businessNews"))