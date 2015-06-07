\name{eta}
\Rdversion{1.1}
\alias{eta}

\title{Coverage Probability}
\description{Calculate the coverage probability}
\usage{
eta(p, m, r, method = c("binomial", "poisson", "exact"))
}

\arguments{
  \item{p}{Total number of available features.}
  \item{m}{Number of features to be drawn by each KNN.}
  \item{r}{Number of KNN  to be generated.}
  \item{method}{Either binomial approximation, poisson approximaton or exact method.}
}
\details{
``exact'' method needs gmp pakage and is slow for large \code{p}.
}
\value{
  The coverage probability is returned.
}

\author{
Shengqiao Li<lishengqiao@yahoo.com>
}

\seealso{
\code{\link{r}}
}
\examples{
 eta(1000, 32, 100)
}

\keyword{distribution}
