\name{rknn}
\alias{rknn}
\alias{rknnReg}
\alias{rknn.cv}
\title{Random KNN Classification and Regression}

\description{Random KNN Classification and Regression}

\usage{
rknn(data, newdata, y, k = 1, r = 500, mtry = trunc(sqrt(ncol(data))), 
    Random.seed = NULL, seed = NULL, knn.algo="VR")
rknn.cv(data, y, k = 1, r = 500, mtry=trunc(sqrt(ncol(data))),
	Random.seed = NULL, seed = NULL, knn.algo="VR")
}

\arguments{
  \item{data}{A training dataset.}
  \item{newdata}{A testing dataset.}
  \item{y}{A vector of responses.}
  \item{k}{Number of nearest neighbors.}
  \item{r}{Number of KNNs.}
  \item{mtry}{Number of features to be drawn for each KNN.}
  \item{Random.seed}{A seed in the \code{.Random.seed} format.}
  \item{seed}{An integer seed.}
  \item{knn.algo}{nearest neighbor searching algorithm.}
}

\value{Return a \code{RandomKNN} object.}

\author{
Shengqiao Li<shli@stat.wvu.edu>
}
\keyword{multivariate}
\keyword{models}
