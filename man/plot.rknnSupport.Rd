\name{plot rknn support}
\alias{plot.rknnSupport}

\title{Plot Function for Support Criterion}

\description{Plot \code{support} of the important features.}
\usage{
\method{plot}{rknnSupport}(x, n.var = min(30, length(x$support)),
    main = deparse(substitute(x)), bg = "gold", lcolor = "blue", ...)
}

\arguments{
  \item{x}{a list with \code{support} returned from \code{rknnSupport}.}
  \item{n.var}{number of variables to be displayed.}
  \item{main}{text for main title.}
  \item{bg}{background color.}
  \item{lcolor}{line color.}
  \item{\dots}{addtional \code{plot} arguments.}
}

\author{
 Shengqiao Li<shli@stat.wvu.edu>
}
\keyword{hplot}
