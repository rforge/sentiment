\name{assignValues}
\alias{assignValues}
\title{Assign Values as Attributes to xmlNode}
\usage{
  assignValues(t, FUN, threshold = 0.5,
    attribname = "attrib", recursive = TRUE,
    mintextlen = 10, ...)
}
\arguments{
  \item{t}{object of class xmlNode}

  \item{FUN}{Function to be executed}

  \item{threshold}{maximum threshold needed to step down
  the tree, defaults to 0.5}

  \item{attribname}{Name of used attribute, defaults to
  "attrib"}

  \item{recursive}{should tree be recursively annotated?,
  defaults to TRUE}

  \item{mintextlen}{minimum textlength needed to step down
  the tree}

  \item{...}{additional arguments for FUN}
}
\description{
  Assign Values as Attributes to xmlNode
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{extractContentDOM}}, \code{\link{xmlNode}}
}

