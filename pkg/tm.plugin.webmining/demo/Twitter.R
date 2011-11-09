
#### retrieve corpus from Twitter Search API for the search Term 'Microsoft'
demo2corp <- Corpus(TwitterSource("Microsoft"))

#inspect elements of retrieved corpus
inspect(demo2corp)

#check meta data of first element in the corpus
meta(demo2corp[[1]])