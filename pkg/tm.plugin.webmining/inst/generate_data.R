# TODO: Add comment
# 
# Author: mario
###############################################################################

library(tm.plugin.webmining)

yahoonews <- WebCorpus(YahooNewsSource("Microsoft"))
yahoonews <- tm_map(yahoonews, removeNonASCII)
save(yahoonews, file = "/home/mario/Dropbox/Private/workspace/sentiment/pkg/tm.plugin.webmining/data/yahoonews.rda")

