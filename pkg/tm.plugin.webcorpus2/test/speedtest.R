# TODO: Add comment
# 
# Author: mario
###############################################################################

library(tm.plugin.webmining)
library(RCurl)


curlOpts = curlOptions(followlocation = TRUE, maxconnects = 20, maxredirs = 5, timeout.ms = 10000)
system.time(tune1corp <- Corpus(YahooNewsSource("Steve+Jobs")))
length(which(sapply(tune1corp, nchar) > 0))/length(tune1corp)

