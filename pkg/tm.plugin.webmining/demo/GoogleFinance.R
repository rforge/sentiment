
#### retrieve corpus from Google Finance News for Microsoft stock
demo1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))

#inspect elements of retrieved corpus
inspect(demo1corp)

#check meta data of first element in the corpus
meta(demo1corp[[1]])

