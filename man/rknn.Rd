\name{rknn}
\alias{rknn}
\alias{rknnReg}
\alias{rknn.cv}
\title{Random KNN Classification and Regression}

\description{Random KNN Classification and Regression}

\usage{
rknn(data, newdata, y, k = 1, r = 500, mtry = trunc(sqrt(ncol(data))), 
        cluster = NULL, seed = NULL)
rknn.cv(data, y, k = 1, r = 500, mtry=trunc(sqrt(ncol(data))), 
        cluster=NULL, seed = NULL)
rknnReg(data, newdata, y, k=1, r=500,  mtry=trunc(sqrt(ncol(data))), 
        cluster=NULL, seed=NULL)	
}

\arguments{
  \item{data}{A training dataset.}
  \item{newdata}{A testing dataset.}
  \item{y}{A vector of responses.}
  \item{k}{Number of nearest neighbors.}
  \item{r}{Number of KNNs.}
  \item{mtry}{Number of features to be drawn for each KNN.}
  \item{cluster}{An object of class `c("SOCKcluster", "cluster")'}
  \item{seed}{An integer seed.}
}

\value{Return a \code{RandomKNN} object.}

\author{
Shengqiao Li<lishengqiao@yahoo.com>
}
\keyword{multivariate}
\keyword{models}
