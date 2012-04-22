\name{r}
\alias{r}
\title{Choose number of KNNs}

\description{Choose number of KNNs}

\usage{
r(p, m = floor(sqrt(p)), eta = 0.99, nu = 20, rmax = p, nsim = 1000,
    lambda = 0.01, method = c("binomial", "poisson", "nu", 
    "geo.independent", "geo.sim", "geo.dependent", "lambda"))
}
\arguments{
  \item{p}{Total number of available features.}
  \item{m}{Number of features to be drawn by each KNN.}
  \item{eta}{Coverage Probability.}
  \item{nu}{mean mutiplicity of a feature}
  \item{rmax}{number of series terms for independent geometric approximation}
  \item{nsim}{number of simulations for geometric simulation.}
  \item{lambda}{mean number of silient features.}
  \item{method}{ one of binomial, poisson, nu, geo.independent, geo.sim, geo.dependent, lambda}
}
\details{
    Method \code{binomial} and \code{poisson} are approximation method for a given \code{eta} value.
    
    Method \code{nu} computes \code{r} for a given \code{nu}.
    
    Method \code{geo.independent}, \code{geo.sim} and \code{geo.dependent} compute \code{r} using
    geomerical random variables \code{z} until eachh is at least drawn once. The difference is that 
    \code{geo.independent} ignores dependency,\code{geo.sim} is a simulation method and \code{geo.independent}
     is an exact method using \code{gmp} package.
    
    Method \code{lambda} computes \code{r} for a given \code{lambda}.
}
\value{
  An integer.
}

\author{
Shengqiao Li

Maintainer: Shengqiao Li<shli@stat.wvu.edu>
}

\examples{
  r(100, 10);
} 
\keyword{distribution}
