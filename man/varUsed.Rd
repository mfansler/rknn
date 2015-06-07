\name{varUsed}
\alias{varUsed}
\alias{varNotUsed}
\title{Features Used or Not Used in Random KNN}

\description{List the features used or not used in Random KNN modeling.}

\usage{
varUsed(x, by.KNN = FALSE, count = TRUE)
varNotUsed(x)
}

\arguments{
  \item{x}{A \code{randomKNN} object.}
  \item{by.KNN}{Logical. Should list features used in each KNN?}
  \item{count}{Logical. Should list number of times each used.}
}

\value{
  \code{varNotUsed} returns a list features not used.
  \code{varUsed} returns a list of feartures used.
}


\author{
Shengqiao Li<lishengqiao@yahoo.com>
}

\keyword{multivariate}

