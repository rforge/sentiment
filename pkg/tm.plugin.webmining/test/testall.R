# TODO: Add comment
# 
# Author: mario
###############################################################################

#install.packages("tm.plugin.webcorpus2", repos="http://R-Forge.R-project.org")
library(tm.plugin.webcorpus2)


test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))

test2corp <- Corpus(TwitterSource("Microsoft"))

nytimes_appid <- "97ce292e8c9fc5d3783492105a837e31:17:60868396"
test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))

test4corp <- Corpus(YahooInplaySource())

test5corp <- Corpus(YahooFinanceSource("MSFT"))

test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))

bing_appid = "CA39C5D6E3FA72F9A68B08991A7BDDBEBE718AEF"
test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))


