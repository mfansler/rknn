################################################################################
# Random KNN Internal Functions                                                #
# These internal functions are not intent for end users                        #
# File:   internal_functions.R                                                 #
# Author: Shengqiao Li                                                         #
# Date:   June 24, 2008 (initial)                                              #
################################################################################

factorial.bigz<- function(n)
{
    if(n==0) return(1);
    prod.bigz(as.bigz(1:n))
}

chooses.bigz<- function(n, m)
  {
    if(m>n) return(0)
    else if(m==0||m==n) return(1)

    res<- prod.bigz(as.bigz(n:(n-m+1)))/prod.bigz(as.bigz(1:m));

    as.bigz(res);
}

choose.bigz<- function(n, m)
{
    ml<- length(n);
    nl<- length(m);
    vl<- max(ml, nl);

    #recycle the short one
    if(ml<vl) n<- rep(n, length=vl)
    else if(nl<vl) m<- rep(m, length=vl)

    res<- as.bigz(vl)

    for(i in 1:vl) res[i]<- chooses.bigz(n[i], m[i])

    return(res)
}

set.return.seed<- function(Random.seed=NULL, seed=NULL)
{
  #set and record random seeds even no seed is supplied
  #set.seed only accepts single integer seed and don't return seed
  #Random.seed  -- a seed in the .Random.seed format
  #seed  -- an integer seed

  if(!is.null(Random.seed)) {
    assign(".Random.seed", Random.seed, envir=.GlobalEnv)
  }
  else  if(!is.null(seed))  set.seed(seed)
  else if(!exists(".Random.seed", envir=.GlobalEnv)) runif(1);

  Random.seed<- get(".Random.seed", envir=.GlobalEnv);

  invisible(Random.seed);
}

#########################################################################
