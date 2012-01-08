
#### retrieve corpus from Google Finance News for Microsoft stock
require(tm.plugin.webmining)
demo1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
demo2corp <- demo1corp
demo1corp <- demo1corp[1:10]

test <- update(demo1corp)

newdata <- demo2corp[!id2 %in% id1]




#inspect elements of retrieved corpus
inspect(demo1corp)

#check meta data of first element in the corpus
meta(demo1corp[[1]])

