\name{internal functions}
\alias{factorial.bigz}
\alias{choose.bigz}
\alias{chooses.bigz}
\alias{rbyb}
\alias{rbyp}
\alias{rbyv}
\alias{rbyz}
\alias{rbyz.sim}
\alias{rbyz.geo}
\alias{rbylambda}

\alias{set.return.seed}
\alias{rknn.FNN}
\alias{rknn.dist}
\alias{backward.rknn.cv}
\alias{rknn.kf.xcv}
\alias{rknn.xcv}
\alias{rknn.pxcv}
\alias{kpart}
\alias{kspart}
\alias{pressresid}

\title{Random KNN Internal Functions}
\description{Some internal and under-development functions
}
\usage{
    factorial.bigz(n)
    chooses.bigz(n, m)
    choose.bigz(n, m)
    set.return.seed(Random.seed=NULL, seed=NULL)
    
    rbyb(p, m, eta)
    rbyp(p, m, eta)
    rbyv(p, m, nu)
    rbyz(p, m)
    rbyz.sim(p, m, nsim=1000)
    rbyz.geo(p, m=floor(sqrt(p)), rmax=p)
    rbylambda(p, m, lambda=1)
    
%    kpart(samples, k=10)
%    kspart(cl, k=10)
%    rknn.kf.xcv(data, y, k=1, r=500, mtry=trunc(sqrt(ncol(data))),
%	              fixed.partition=FALSE, pk=0.5, K=10, Random.seed = NULL, seed = NULL,
%           	  knn.algo = "VR", criterion=c("mean_accuracy", "mean_support"))
%  	rknn.xcv(data, y, k=1, r=500, mtry=trunc(sqrt(ncol(data))),
%	          fixed.partition=FALSE,pk=0.5, Random.seed = NULL, seed = NULL,
%	          knn.algo = "VR", criterion=c("mean_accuracy", "mean_support"))
%   rknn.pxcv(data, y, k=1, r=500, mtry=trunc(sqrt(ncol(data))),
%	           fixed.partition=FALSE,pk=0.5, Random.seed = NULL, seed = NULL,
%	           knn.algo = "VR", criterion=c("mean_accuracy", "mean_support"))
    rknn.FNN(data, y, k=5, r=500, mtry=trunc(sqrt(ncol(data))))
    rknn.dist(data, r=500, k=1, mtry=trunc(sqrt(ncol(data))), y=NULL)
    pressresid(obj)
    
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of elements in a set chosen from.}
  \item{m}{Number of elements in a subset to be drawn.}
  \item{p}{Total number of available features.}  
  \item{mtry}{Number of features to be drawn for each KNN.}
  \item{eta}{Coverage Probability.}
  \item{nu}{mean mutiplicity of a feature}
  \item{rmax}{number of series terms for independent geometric approximation}
  \item{nsim}{number of simulations for geometric simulation.}
  \item{lambda}{mean number of silient features.}
  \item{samples}{A vector of indice for a set of observations.}
  \item{cl}{A factor for classification labels.}
  \item{data}{A data matrix.}  
  \item{y}{A vector of responses.}
  \item{k}{Number of nearest neighbors.}
  \item{K}{Number of folds for cross-validation.}
  \item{pk}{A real number between 0 and to indicate the proportion of the feature set to be kept in each step.}
  \item{r}{Number of KNN  to be generated.}
  \item{Random.seed}{A seed in the \code{.Random.seed} format.}
  \item{seed}{An integer seed.}
  \item{criterion}{either uses mean_accuracy or mean_support for best.}
  \item{obj}{A linear model.}
} 
\author{
Shengqiao Li<shli@stat.wvu.edu>
}

 
\keyword{internal}
