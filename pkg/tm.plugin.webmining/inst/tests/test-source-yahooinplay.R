# TODO: Add comment
# 
# Author: mario
###############################################################################

context("YahooInPlaySource")

test_that("YahooInPlaySource",{
	
	lengthcorp <- 10
		
	testcorp <- WebCorpus(YahooInplaySource())
	# Check Corpus object
	expect_that(length(testcorp) >= 10, is_true())
	expect_that(class(testcorp), equals(c("WebCorpus","VCorpus","Corpus","list")))
	
	# Check Content
	expect_that(all(sapply(testcorp, nchar) > 0), is_true())
	
	# Check Meta Data
	datetimestamp <- lapply(testcorp, function(x) meta(x, "DateTimeStamp"))
	#FIXME: Date should be fixed
	expect_that(all(sapply(datetimestamp, function(x) class(x)[1] == "character")), is_true())
	
	heading <- lapply(testcorp, function(x) meta(x, "Heading"))
	expect_that(all(sapply(heading, function(x) class(x)[1] == "character")), is_true())
	expect_that(all(sapply(heading, nchar) > 0), is_true())
	
	id <- lapply(testcorp, function(x) meta(x, "ID"))
	expect_that(all(sapply(id, function(x) class(x)[1] == "character")), is_true())
	expect_that(all(sapply(id, nchar) > 0), is_true())
	
	testcorp <- testcorp[1:10]
	testcorp <- corpus.update(testcorp)
	expect_that(length(testcorp) >= lengthcorp, is_true())
})

