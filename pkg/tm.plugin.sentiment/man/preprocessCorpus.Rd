\name{preprocessCorpus}
\alias{preprocessCorpus}
\title{Preprocess Corpus with functions specified in control.
Little helper function to have a little more control of the preprocessing step.
Especially the order of the control elements is preserved (as opposed to the
preprocessing done in DocumentTermMatrix or TermDocumentMatrix.}
\usage{
  preprocessCorpus(corpus, control, verbose = FALSE)
}
\arguments{
  \item{corpus}{\code{\link{Corpus}} object which should be
  processed}

  \item{control}{list of functions to be applied to corpus}

  \item{verbose}{print preprocessing status information}
}
\description{
  Preprocess Corpus with functions specified in control.
  Little helper function to have a little more control of
  the preprocessing step. Especially the order of the
  control elements is preserved (as opposed to the
  preprocessing done in DocumentTermMatrix or
  TermDocumentMatrix.
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{DocumentTermMatrix}}
  \code{\link{TermDocumentMatrix}}
}

