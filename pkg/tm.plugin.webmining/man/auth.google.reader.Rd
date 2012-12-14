\name{auth.google.reader}
\alias{auth.google.reader}
\title{Authentification with the Google Reader Web Service}
\usage{
  auth.google.reader(email = readline("Email:"),
    password = readline("Password:"), get.curl.opts = F)
}
\arguments{
  \item{email}{email address, e.g.
  <firstname.lastname@gmail.com>}

  \item{password}{password for Google Account}

  \item{get.curl.opts}{determines if RCurl options object
  (\code{\link{curlOptions}}) should be returned.}
}
\value{
  Character if get.curl.opts is FALSE or curlOpts if
  get.curl.opts is TRUE
}
\description{
  Authentification with the Google Reader Web Service
}

