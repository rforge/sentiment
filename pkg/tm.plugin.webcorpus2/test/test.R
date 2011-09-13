# TODO: Add comment
# 
# Author: mario
###############################################################################


library(tm.plugin.webcorpus2)

test <- GoogleFinanceSource("NASDAQ:MSFT")

###test google finance source
test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
test2corp <- Corpus(TwitterSource("Microsoft"))

