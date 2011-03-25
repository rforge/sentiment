\name{getFeed}
\alias{getFeed}
\title{Get (Meta-)Content list from Feed .}
\usage{getFeed(feed, params, fieldnames, dateConvertFields=c("DateTimeStamp"),
    htmlConvertFields=c("Description"), timeFormat, timeZone="GMT",
    type="XML", extractor=extractHTMLStrip, verbose=TRUE, ...)
}
\description{Get (Meta-)Content list from Feed .
After feed is retrieved Conversion of Date and HTML Content fields is done}
\author{Mario Annau}
\alias{getFeed}
\value{named list}
\seealso{\code{\link{xmlTreeParse}} \code{\link{strptime}} \code{\link{getCorpus}}}
\note{for examples see \code{\link{getCorpus}}}
\arguments{\item{feed}{character (vector) of feeds to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)}
\item{params}{named list of feed parameters}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{dateConvertFields}{fields to convert to POSIXt, defaults to "pubDate"}
\item{htmlConvertFields}{fields which contain html syntax and should be converted to text (with tags removed), 
defaults to "description"}
\item{timeFormat}{character defining time format by ISO C99 / POSIX standard for \code{\link{strptime}},
defaults to "\%a, \%d \%b \%Y \%H:\%M:\%S"}
\item{timeZone}{character defining time zone, defaults to GMT}
\item{type}{Specify feed type.
#' Already implemented types are:
\itemize{
\item{\code{XML}} Get (Meta-)Content list from XML, see \code{\link{getFeed.XML}}
\item{\code{RSS}} Get (Meta-)Content list from RSS Feed, see \code{\link{getFeed.RSS}}
\item{\code{ATOM}} Get (Meta-)Content list from ATOM Feed, see \code{\link{getFeed.ATOM}}
\item{\code{JSON}} Get (Meta-)Content list from JSON, see \code{\link{getFeed.JSON}} 
\item{\code{CSV}} Get (Meta-)Content list from CSV, see \code{\link{getFeed.CSV}}
}}
\item{extractor}{Text extraction function used for \code{htmlConvertFields}}
\item{verbose}{Print feed retrieval status to console, defaults to TRUE}
\item{...}{Addtional parameters for \code{getFeed.#type#} function}
}

