# TODO: Add comment
# 
# Author: mario
###############################################################################

#install.packages("tm.plugin.webcorpus2", repos="http://R-Forge.R-project.org")
library(tm.plugin.webcorpus2)

test <- GoogleFinanceSource("NASDAQ:MSFT")

###test google finance source
test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
test2corp <- Corpus(TwitterSource("Microsoft"))

