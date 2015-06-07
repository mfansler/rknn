\name{fitted}
\alias{fitted.rknn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Model Fitted Values}

\description{Extract Random KNN fitted values.}

\usage{
\method{fitted}{rknn}(object, ...)
}

\arguments{
  \item{object}{A rknnobject.}
  \item{\dots}{Additional arguments.}
}

\value{
  A vector of fitted values.
}
\author{
Shengqiao Li<lishengqiao@yahoo.com>
}

\examples{
\dontrun{
  golub.train<- t(scale(golubTrain));
  golub.test<- t(scale(golubTest)); 
  golub.rnn<- randomKNN(data=golub.train, newdata=golub.test, y=golubTrain.cl,
     r=821, mtry=55);
  fitted(golub.rnn)
}
}
\keyword{models}
