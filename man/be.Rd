\name{rknnBeg}
\alias{rknnBeg}
\alias{rknnBel}
\title{Backward Elimination Feature Selection with Random KNN}
\description{
Recursive Backward Elimination Feature Selection with Random KNN
}
\usage{
rknnBeg(data, y, k = 1, r = 500, mtry = trunc(sqrt(ncol(data))),
  fixed.partition = FALSE, pk = 0.5, stopat = 4,
  Random.seed = NULL, seed = NULL, knn.algo="VR")
rknnBel(data, y, k = 1, r = 500, mtry = trunc(sqrt(ncol(data))),
  fixed.partition = FALSE, d = 1, stopat = 4,
  Random.seed = NULL, seed = NULL, knn.algo="VR")
}
\arguments{
  \item{data}{An n x p numeric design matrix.}
  \item{y}{A vector of responses. For a numeric vector,  Random Knn regression
   is performed. For a factor, Random classification is performed.}
  \item{k}{An integer for the number of nearest neighbors.}
  \item{r}{An integer for the number of base KNN models.}
  \item{mtry}{Number of features to be drawn for each KNN.}
  \item{fixed.partition}{Logical. Use fixed partition of dynamic partition of the data into training and testing subsets for each KNN.
}

  \item{pk}{A real number between 0 and to indicate the proportion of the feature set to be kept in each step.}
  \item{d}{A integer to indicate the number of features to be dropped in each step.}
  \item{stopat}{an integer for the  minimum number of variables.}
  \item{Random.seed}{A seed for \code{RNG} in \code{.Random.seed} format.}
  \item{seed}{An integer seed.}
  \item{knn.algo}{nearest neighbor searching algorithm.}
}


\author{
Shengqiao Li<shli@stat.wvu.edu>
}

\keyword{classif}
\keyword{regression}
\keyword{multivariate}
