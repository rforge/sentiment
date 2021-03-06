# TODO: Add comment
# 
# Author: mario
###############################################################################

context("GoogleFinanceSource")

test_that("GoogleFinanceSource",{
	
	lengthcorp <- 20
		
	testcorp <- WebCorpus(GoogleFinanceSource("NASDAQ:MSFT"))
	# Check Corpus object
	expect_that(length(testcorp), equals(lengthcorp))
	expect_that(class(testcorp), equals(c("WebCorpus","VCorpus","Corpus")))
	
	# Check Content
	#expect_that(all(sapply(testcorp, nchar) > 0), is_true())
	contentratio <- length(which(sapply(testcorp, nchar)[1,] > 0)) / length(testcorp)
	expect_that(contentratio > 0.5, is_true())
	
	# Check Meta Data
	datetimestamp <- lapply(testcorp, function(x) meta(x, "datetimestamp"))
	expect_that(all(sapply(datetimestamp, function(x) class(x)[1] == "POSIXlt")), is_true())
	
	description <- lapply(testcorp, function(x) meta(x, "description"))
	expect_that(all(sapply(description, function(x) class(x)[1] == "character")), is_true())
	
	heading <- lapply(testcorp, function(x) meta(x, "heading"))
	expect_that(all(sapply(heading, function(x) class(x)[1] == "character")), is_true())
	expect_that(all(sapply(heading, nchar) > 0), is_true())
	
	id <- lapply(testcorp, function(x) meta(x, "id"))
	expect_that(all(sapply(id, function(x) class(x)[1] == "character")), is_true())
	expect_that(all(sapply(id, nchar) > 0), is_true())
	
	origin <- lapply(testcorp, function(x) meta(x, "origin"))
	expect_that(all(sapply(origin, function(x) class(x)[1] == "character")), is_true())
	expect_that(all(sapply(origin, nchar) > 0), is_true())
	
	testcorp <- testcorp[1:10]
	testcorp <- corpus.update(testcorp)
	expect_that(length(testcorp) >= lengthcorp, is_true())
	
	cat(" | Contentratio: ", sprintf("%.0f%%", contentratio * 100))
})

