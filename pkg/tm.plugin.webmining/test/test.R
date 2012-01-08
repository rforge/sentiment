# TODO: Add comment
# 
# Author: mario
###############################################################################

#install.packages("tm.plugin.webcorpus2", repos="http://R-Forge.R-project.org")
library(tm.plugin.webmining)
#nytimes_appid must be defined
#bing_appid must be defined

test1corp <- WebCorpus(GoogleFinanceSource("NASDAQ:MSFT"))
test1corp <- test1corp[1:10]
#all meta data is still stored in corpus object
summary(CMetaData(test1corp)[["MetaData"]])
#and now comes the trick...
test1corp <- tm.update(test1corp)

###the same update procedure works for the following corpora:
test2corp <- Corpus(TwitterSource("Microsoft"))
test2corp <- test2corp[1:10]
test2corp <- update(test2corp)

test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))
test3corp <- test3corp[1:10]
test3corp <- update(test3corp)

test4corp <- Corpus(YahooInplaySource())
test4corp <- test4corp[1:10]
test4corp <- update(test4corp)

test5corp <- Corpus(YahooFinanceSource("MSFT"))
test5corp <- test5corp[1:10]
test5corp <- update(test5corp)

test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))
test6corp <- test6corp[1:10]
test6corp <- update(test6corp)

test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))
test7corp <- test7corp[1:10]
test7corp <- update(test7corp)

test8corp <- Corpus(ReutersNewsSource("businessNews"))
test8corp <- test8corp[1:10]
test8corp <- update(test8corp)



