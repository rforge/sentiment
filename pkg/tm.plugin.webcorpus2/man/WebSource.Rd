\name{WebSource}
\alias{WebSource}
\title{Read Web Content and respective Link Content from feedurls...}
\usage{WebSource(feedurls, parser, linkreader, getLinkContent=!missing(linkreader),
    encoding="UTF-8")
}
\description{Read Web Content and respective Link Content from feedurls}
\arguments{\item{feedurls}{urls from feeds to be retrieved}
\item{parser}{function to be used to split feed content into chunks, returns list of content elements}
\item{linkreader}{function to be applied to content elements from parser, returns character vector of urls}
\item{getLinkContent}{specifies whether urls from linkreader should be retrieved}
\item{encoding}{specifies default encoding, defaults to 'UTF-8'}
}

