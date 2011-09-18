# TODO: Add comment
# 
# Author: mario
###############################################################################

#install.packages("tm.plugin.webcorpus2", repos="http://R-Forge.R-project.org")
library(tm.plugin.webcorpus2)

test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))

test2corp <- Corpus(TwitterSource("Microsoft"))

test3corp <- Corpus(NYTimesSource("Microsoft", appid = appid))

test4corp <- Corpus(YahooInplaySource())

test5corp <- Corpus(YahooFinanceSource("MSFT"))

test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))

test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))