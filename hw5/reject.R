library(parallel)

reject = function(n, dfunc, range, mc){ 
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
  
  ## 1) generate values of Xs and Ys from uniform
  # samples.uniform<-NULL
  #   for( i in 1:n){
  #   proposal<-runif(n, min(range), max(range))
  #   density.ratio<-dfunc(proposal)/dunif(proposal)
  #   if(proposal<density.ratio) samples.uniform <-c(samples.uniform,proposal)
  # }
  
  rejection=function(n,dfunc,range,mc){  
    proposal_x=runif(n, min(range), max(range))
    ys<-dfunc(x=proposal_x)
    proposal_y<-runif(n, min=min(ys), max=max(ys)) #envelop function
    
    
    ## 2) reject if Y is larger than the value from dfunc
    result_x=rep(0,n)
    result_x[1]=proposal_x[1]
    for(i in 2:n) {
      x=proposal_x[i]      
      y=dfunc(x) #target dist. evaluated at x
      proposal_int=proposal_y[i] #envelop dist. evaluated at x
      ratio=y/proposal_int # calculate the ratio
      if(runif(1,0,1)<ratio){ 
        result_x[i]=x
      } else{result_x[i]=result_x[i-1]} # end of acceptance condition
    } #end of rejection for loop
    return(result_x)
  }
  
  vec=unlist(mclapply(1:mc, function(x) rejection(ceiling(n/mc),dfunc,range,mc),mc.cores=mc))
  vec=vec[1:n]
  return(vec)
  
}  # end of function
