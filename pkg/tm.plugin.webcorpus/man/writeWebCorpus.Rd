\name{writeWebCorpus}
\alias{writeWebCorpus}
\title{Write list containing web corpus to disk...}
\usage{writeWebCorpus(corpuslist, html_source, sourcefield, path, extractdir="extract",
    sourcedir="source", idxnum=1:length(corpuslist[[1]]))
}
\description{Write list containing web corpus to disk}
\note{This function is still in an experimental status}
\arguments{\item{corpuslist}{list containing corpus fields}
\item{html_source}{list containing html sources of the corpus}
\item{sourcefield}{field specifying sourcefield in corpuslist which will not be used in xml index}
\item{path}{to be used for corpus list writing}
\item{extractdir}{character, specifying directory which holds extracted text elements, defaults to "extract"}
\item{sourcedir}{character, specifying directory which holds extracted text elements, defaults to "source"}
\item{idxnum}{index numbers to be used for elements, defaults to \code{1:length(corpuslist[[1]])}}
}

