
#### retrieve corpus from Google Blog Search for the search Term 'Microsoft'
demo3corp <- Corpus(GoogleBlogSearchSource("Microsoft"))

#inspect elements of retrieved corpus
inspect(demo3corp)

#check meta data of first element in the corpus
meta(demo3corp[[1]])

