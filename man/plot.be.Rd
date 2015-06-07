\name{plot backward elimination}
\alias{plot.rknnBeg}
\alias{plot.rknnBel}

\title{Plot Function for Recursive Backward Elimination Feature Selection}
\description{Plot the  recursive backward elimination feature selection process.}

\usage{
\method{plot}{rknnBeg}(x, col = "springgreen4", xlab = "no. of features", ylab = "mean accuracy", ...)
\method{plot}{rknnBel}(x, col = "springgreen4", xlab = "no. of features", ylab = "mean accuracy", ...)
}
\arguments{
  \item{x}{An object returned from \code{rknnBel} or \code{rknnBeg}.}
  \item{col}{Plot line color.}
  \item{xlab}{Plot x label.}
  \item{ylab}{Plot x label.}
  \item{\dots}{Addional plot parameters.}
}

\author{
Shengqiao Li

Maintainer: Shengqiao Li<lishengqiao@yahoo.com>
}
\keyword{hplot}
