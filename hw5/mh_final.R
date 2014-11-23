mh = function(n, dfunc, range, mc=FALSE){
  library(parallel)
  library(doMC)
  library(foreach)
  
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
  
  min=min(range)
  max=max(range)
  
  tuning=function(dfunc,min,max)
  {
    v=1.5
    tune=0.1
    while(tune<0.3 || tune>0.4){
      if(tune<0.1)
      {
        v=v/1.2
      } else 
      {
        if(tune>=0.1 && tune<0.3)
        {
          v=v/1.1
        } else 
          {
            if(tune>0.55)
            {
              v=v*1.25
            } else 
              {
                if(tune<=0.55 && tune>0.4)
                {
                  v=v*1.15
                }
              }
          }
      }
      
      count=0
      tunmat=matrix(0,200,1)
      tunmat[1]=runif(1,min,max)
      
      for(i in 2:200) 
      {
        prop=rnorm(1,tunmat[i-1],v)
        
        if(prop<min || prop>max)
        {
          tunmat[i]=tunmat[i-1] 
        } else{
          acprob=min(1,dfunc(prop)/dfunc(tunmat[i-1]))
          sam=runif(1,0,1)
          if(sam<=acprob)
          {
            tunmat[i]=prop
            count=count+1
          } else
          {tunmat[i]=tunmat[i-1]
          }
        }
      }  
      tune=count/200
    } 
    return(v)
  }
  v=tuning(dfunc,min,max)
  
  sampling=function(n,dfunc,min,max,v){
    
    x=matrix(0,n,1)
    x[1]=runif(1,min,max)
    
    for(i in 2:n) 
    {
      prop=rnorm(1,x[i-1],v)
      
      if(prop<min || prop>max)
      {
        x[i]=x[i-1]
      } else{
        accprob=min(1,dfunc(prop)/dfunc(x[i-1]))
        sam=runif(1,0,1)
        if(sam<=accprob)
        {
          x[i]=prop
        } else
        {x[i]=x[i-1]}
      }
    }
    return(x)}
  vec=unlist(mclapply(1:mc, function(x) sampling(ceiling(n/mc),dfunc,min,max,v),mc.cores=mc))
  vec=vec[1:n]
  return(vec)
}


