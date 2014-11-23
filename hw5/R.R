### R function explanation

firstly, detecting the type of function by using as.charactor(substitute()).

Once it defines the type of function,
each function follows the logic below.

for (beta), we can use (rbeta) function for sampling

for (truncated exponential), firsty, making an empty vector
and then generate and save samples by using (rnorm) function, if conditions are met

for (truncated exponential), sampling method is similar to the above (truncated normal).

for (uniform mixture), after making an empty vector, we can generate probablity from (runif) function.
and then if probabilty <= 0.6, generate sample from runif(1,-3,-1). 
if 0.6<probablity<=0.7, generate sample from runif(1,-1,1). else generate from runif(1,1,4)

for (truncated normal mixture1), after making an empty vector, in the same way above, we can generate probablity from (runif) function.
ans then if probablity <= 0.5, generate sample from rnorm(1,2,2). else generate from rnorm(1,6,1).

for (truncated normal mixture2), sampling method is similar to the above (truncated normal mixture1)



### R function 

R = function(n,dfunc,range,mc){
  
  library(parallel)
  library(doMC)
  library(foreach)
  library(truncnorm)
  
  stopifnot(is.function(dfunc))
  stopifnot(is.numeric(n))
  stopifnot(is.logical(mc))
  stopifnot(is.vector(range))
  
  if(mc==FALSE){
    mc=1
  } else{
    if(n>10000)
    {
      mc=8
    } else{
      mc=2
    }
  }
  
  if(as.character(substitute(dfunc))=="dbetann"){
    return(unlist(mclapply(1:mc,function(x) rbeta(ceiling(n/mc),0.9,0.9),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtnorm"){
    return(unlist(mclapply(1:mc,function(x) rtruncnorm(ceiling(n/mc),-3,3),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtexp"){
    exp=function(n){
      v=rep(0,n)
    for(i in 1:n) {
      x=-1
      while(x < 0 || x > 6){ 
        x=rexp(1,rate=1/3)
        v[i]=x}}
    return(v)}
    return(unlist(mclapply(1:mc, function(x) exp(ceiling(n/mc)),
                           mc.cores=mc)))
  }
  
  if(as.character(substitute(dfunc))=="dunif_mix"){
    return(unlist(mclapply(1:mc, function(x) c(runif(ceiling(0.6*n/mc),-3,-1),
                                                  runif(ceiling(0.1*n/mc),-1,1),
                                                  runif(ceiling(0.3*n/mc), 1,4)),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtnorm_mix1"){
    return(unlist(mclapply(1:mc, function(x) c(rtruncnorm(ceiling(0.5*n/mc),0,10,2,2),
                                                  rtruncnorm(ceiling(0.5*n/mc),0,10,6,1)),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtnorm_mix2"){
    return(unlist(mclapply(1:mc,
                           function(x) c(rtruncnorm(ceiling(0.45*n/mc),-4,4,-4),
                                         rtruncnorm(ceiling(0.45*n/mc),-4,4,4),
                                         rtruncnorm(ceiling(0.1*n/mc),-4,4,0,0.5)),
                           mc.cores=mc)))
  }
}


