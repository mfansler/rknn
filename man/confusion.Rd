\name{confusion}
\alias{confusion}
\alias{confusion2acc}
\title{Classification Confusion Matrix and Accuracy}
\description{
  Compute classification confusion matrix and accuracy 
}
\usage{
confusion(obs, pred)
confusion2acc(ct)
}

\arguments{
  \item{obs}{A vector of observed classes.}
  \item{pred}{A vector of predicted classes.}
  \item{ct}{A table retruned from \code{confusion}}
}

\value{
  \code{confusion} return a matrix of cross classification counts.
  \code{confusion} return a overall classification accuracy.
}

\author{
Shengqiao Li<lishengqiao@yahoo.com>
}

\examples{
  obs<- rep(0:1, each =5);
  pre<- c(obs[3:10], obs[1:2])
  confusion(obs, pre)
  confusion2acc(  confusion(obs, pre))
}
\keyword{classif}
