\name{internal functions}
\alias{rbyb}
\alias{rbyp}
\alias{rbyv}
\alias{rbyz}
\alias{rbyz.sim}
\alias{rbyz.geo}
\alias{rbylambda}
\alias{backward.rknn.cv}
\alias{rknn.kf.xcv}
\alias{rknn.xcv}
\alias{rknn.pxcv}
\alias{kpart}
\alias{kspart}
\alias{pressresid}
\alias{knn}
\alias{knn.cv}
\alias{knn.reg}
\title{Random KNN Internal Functions}
\description{Some internal and under-development functions
}
\usage{   
    rbyb(p, m, eta)
    rbyp(p, m, eta)
    rbyv(p, m, nu)
    rbyz(p, m)
    rbyz.sim(p, m, nsim=1000)
    rbyz.geo(p, m=floor(sqrt(p)), rmax=p)
    rbylambda(p, m, lambda=1)
    knn(train, test, cl, k=1)
    knn.cv (train, cl, k=1)
    knn.reg(train, test = NULL, y, k = 3)
    pressresid(obj)
    
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{train}{A data matrix.}  
  \item{test}{A data matrix.}  
  \item{y}{A vector of responses.}
  \item{k}{Number of nearest neighbors.}
  \item{cl}{A vector of class labels.}
  \item{K}{Number of folds for cross-validation.}
  \item{pk}{A real number between 0 and to indicate the proportion of the feature set to be kept in each step.}
  \item{r}{Number of KNN  to be generated.}
  \item{seed}{An integer seed.}
  \item{criterion}{either uses mean_accuracy or mean_support for best.}
  \item{obj}{A linear model.}
} 
\author{
Shengqiao Li<lishengqiao@yahoo.com>
}

 
\keyword{internal}
