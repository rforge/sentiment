\name{getCorpus}
\alias{getCorpus}
\title{Get Corpus from a web source.}
\usage{getCorpus(query, src="yahooFinance", fastmode=FALSE,
    extractor="extractContentDOM", extractorOpts=list(), verbose=FALSE,
    getURLPartOpts, useSource=FALSE, sourceField="Source",
    writeCorpus=FALSE, sourcePath=getwd(), ...)
}
\description{Get Corpus from a web source.}
\author{Mario Annau}
\value{corpus}
\seealso{\code{\link{getURLPart}}}
\arguments{\item{query}{user defined (search) query. If query is given as a character vector it will be collapsed with + as separator}
\item{src}{source from which (Meta-) Data should be retrieved from. Internally the function getMeta.#src# will be called.
Already implemented sources are:
\itemize{
\item{\code{yahooFinance}} Yahoo Finance, see \code{\link{getMeta.yahooFinance}}
\item{\code{googleFinance}} Yahoo Finance, see \code{\link{getMeta.googleFinance}}
\item{\code{googleBlogSearch}} Google Blog Search, see \code{\link{getMeta.googleBlogSearch}}
\item{\code{bing}} Bing News Search, see \code{\link{getMeta.bing}} (api-key required)
\item{\code{yahooBOSS}} Yahoo BOSS Search, see \code{\link{getMeta.yahooBOSS}} (api-key required)
\item{\code{spotlight}} Reuters Spotlight, see \code{\link{getMeta.spotlight}} (api-key required)
\item{\code{nytimes.articlesearch}} NY Times Article Search, see \code{\link{getMeta.nytimes.articlesearch}} (api-key required)
\item{\code{twitter}} Twitter Search, see \code{\link{getMeta.twitter}}
}}
\item{fastmode}{Instead of downloading data (defined by "Origin" in meta list) "Description" or "Heading" field is used as Content, defaults to FALSE}
\item{extractor}{Extractor function to be used for downloaded data, defaults to \code{extractContentDOM}}
\item{extractorOpts}{Specify further parameters for the extraction function as list, defaults to list()}
\item{verbose}{print status output to console, defaults to FALSE}
\item{getURLPartOpts}{set default parameters for url download function \code{\link{getURLPart}} as \code{list}}
\item{useSource}{Integrate Source in Corpus in the field \code{sourcefield}, defaults to FALSE}
\item{sourceField}{If useSource is TRUE, websource will be integrated in defined meta data field}
\item{writeCorpus,}{specifies if the corpus and its sources should be written to disk
Please note, that this feature is still in an experimental stage}
\item{sourcePath}{character specifying full path where corpus should be written, will be created if not existent, defaults to \code{getwd()}}
\item{...}{additional parameters for respective getMeta.#src# function}
}
\examples{\dontrun{
corpus <- getCorpus("MSFT", src = "yahooFinance", n = 20)
}}

