\name{bestset}
\alias{bestset}
\alias{prebestset}

\title{Extract the Best Subset of Feature from Selection Process}
\description{Extract the best subset of feature from selection process.}

\usage{
  bestset(x, criterion=c("mean_accuracy", "mean_support"))
  prebestset(x,  criterion=c("mean_accuracy", "mean_support"))
}

\arguments{
  \item{x}{An object returned by \code{rknnBeg} or \code{rknnBel}.}
  \item{criterion}{either uses mean_accuracy or mean_support for best.}
}

\value{
  A character vector of feature names.
}
\author{
 Shengqiao Li<lishengqiao@yahoo.com>
}

\seealso{
 \code{\link{rknnBeg}}, \code{\link{rknnBel}}
}
\examples{
\dontrun{bestset{x}}
}

\keyword{multivariate}

