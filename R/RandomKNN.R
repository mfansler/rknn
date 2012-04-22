################################################################################
# RandomKNN program                                                            #
# File:   RandomKNN.R                                                          #
# Author: Shengqiao Li                                                         #
# Date:   June 24, 2008 (initial)                                              #
# Dependency: class, Hmisc                                                     #
# Change Log:                                                                  #
#           December 11, 2008 - Add set.return.seed                            #
#           January 31, 2009 - Add randomKNN.cv                                #
################################################################################

#imputation method in R
#impute::impute.knn

confusion<- function(obs, pred)
{
  #when print out, "classified as" actually points to pred.
  table(obs, pred, dnn=list("classified as->", ""));

}

confusion2acc<- function(ct)
{
  sum(diag(ct))/sum(ct)
}

cv.coef<- function(x)
#coefficient of variation
{
  sd(x)/mean(x)
}

rknn<- function(data, newdata, y, k=1, r=500, 
              mtry=trunc(sqrt(ncol(data))),
              Random.seed=NULL, seed=NULL, knn.algo="VR"
)
{
  #
  #Note: kknn::kknn is not good as class::knn. Low accuracy for Golub data
  #      klaR::sknn is even worse
  # 
   if(!require(FNN)) stop("FNN package is required!");
   
   res<- list(call=match.call());
   res$Random.seed<- set.return.seed(Random.seed, seed);
   
   p<- ncol(newdata);
   
   n<- nrow(newdata);

   res$k<- k;
   res$r<- r;
   res$mtry<- mtry;
   res$n<- n;
   res$p<- p;

   if(!is.factor(y)) y<- as.factor(y);

   selected<- matrix(integer(), nrow=r, ncol=mtry);

   pred.all<- matrix(nrow=n, ncol=r);

   for(j in 1:r){

        fset<- sample(p, mtry);
        selected[j,]<- fset;
        aknn<- knn(train=data[, fset], test=newdata[, fset], cl=y, k=k, algorithm=knn.algo);

        pred.all[,j]<- as.integer(aknn);

   }

  pred<- character(n);
  for(i in 1:n) pred[i]<- names(which.max(table(pred.all[i,])));


  res$pred<- factor(pred, levels = seq_along(levels(y)), labels = levels(y));
  
   #features<- table(selected);

  #names(features)<- colnames(data)[as.integer(names(features))];
  res$features<- if(is.null(colnames(data))){1:p} else colnames(data);
  	
  res$features.used<- selected;
  
  class(res)<- "rknn";

  return(res);
}

rknn.cv<- function(data, y, k=1, r=500, 
          mtry=trunc(sqrt(ncol(data))),
          Random.seed=NULL, seed=NULL,
          knn.algo="VR"
)
{   
   if(!require(FNN)) stop("FNN package is required!");
     
   res<- list(call=match.call());
   res$Random.seed<- set.return.seed(Random.seed, seed);   
   p<- ncol(data);
   n<- nrow(data);

   res$k<- k;
   res$r<- r;
   res$mtry<- mtry;
   res$n<- n;
   res$p<- p;

   if(!is.factor(y)) y<- as.factor(y);

   selected<- matrix(integer(), nrow=r, ncol=mtry);

   pred.all<- matrix(nrow=n, ncol=r);

   for(j in 1:r){

        fset<- sample(p, mtry);
        selected[j,]<- fset;
        aknn<- knn.cv(train=data[, fset], cl=y, k=k, algorithm="VR");

        pred.all[,j]<- as.integer(aknn);

   }

  pred<- character(n);
  for(i in 1:n) pred[i]<- names(which.max(table(pred.all[i,])));


  res$pred<- factor(pred, levels = seq_along(levels(y)), labels = levels(y));

   #features<- table(selected);

  #names(features)<- colnames(data)[as.integer(names(features))];
  res$features<- if(is.null(colnames(data))){1:p} else colnames(data);

  res$features.used<- selected;

  class(res)<- "rknn";

  return(res);
}

varUsed<- function (x, by.KNN = FALSE, count = TRUE)
{
    if (!inherits(x, "rknn"))
        stop(deparse(substitute(x)), "is not a rknn object")

    if (is.null(x$features.used))
        stop(deparse(substitute(x)), "does not contain variables")
    p <- x$p
    if (count) {
        if (by.KNN) {
            v <- apply(x$features.used, 1, function(x) {
                xx <- integer(p)
                y <- table(x)
                xx[as.integer(names(y))] <- y
                xx
            })
            v<- t(v)
        }
        else {
            v <- integer(p)
            vv <- table(x$features.used)
            v[as.integer(names(vv))] <- vv
        }
   }
   else {
        v <- t(apply(x$features.used, 1, function(x) sort(x)));
        if (!by.KNN)
            v <- sort(unique(as.vector(v)));
   }

   v;

}

varNotUsed<- function(x)
{
   if (!inherits(x, "rknn"))
        stop(deparse(substitute(x)), "is not a rknn object")

	index<- setdiff(1:x$p, varUsed(x, by.KNN=FALSE, count=FALSE))

	if(length(index)==0){ NULL} else x$features[index];

}
