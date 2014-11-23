library(xtable)

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

mhs11<-system.time(mh(100,dbetann,c(0,1),FALSE))[3]
mhs12<-system.time(mh(10000,dbetann,c(0,1),FALSE))[3]
mhs13<-system.time(mh(1000000,dbetann,c(0,1),FALSE))[3]
mhs14<-system.time(mh(10000000,dbetann,c(0,1),FALSE))[3]

mhs21<-system.time(mh(100,dtnorm,c(-3,3),FALSE))[3]
mhs22<-system.time(mh(10000,dtnorm,c(-3,3),FALSE))[3]
mhs23<-system.time(mh(1000000,dtnorm,c(-3,3),FALSE))[3]
mhs24<-system.time(mh(10000000,dtnorm,c(-3,3),FALSE))[3]

mhs31<-system.time(mh(100,dtexp,c(0,4),FALSE))[3]
mhs32<-system.time(mh(10000,dtexp,c(0,4),FALSE))[3]
mhs33<-system.time(mh(1000000,dtexp,c(0,4),FALSE))[3]
mhs34<-system.time(mh(10000000,dtexp,c(0,4),FALSE))[3]

mhs41<-system.time(mh(100,dunif_mix,c(-3,4),FALSE))[3]
mhs42<-system.time(mh(10000,dunif_mix,c(-3,4),FALSE))[3]
mhs43<-system.time(mh(1000000,dunif_mix,c(-3,4),FALSE))[3]
mhs44<-system.time(mh(10000000,dunif_mix,c(-3,4),FALSE))[3]

mhs51<-system.time(mh(100,dtnorm_mix1,c(0,10),FALSE))[3]
mhs52<-system.time(mh(10000,dtnorm_mix1,c(0,10),FALSE))[3]
mhs53<-system.time(mh(1000000,dtnorm_mix1,c(0,10),FALSE))[3]
mhs54<-system.time(mh(10000000,dtnorm_mix1,c(0,10),FALSE))[3]

mhs61<-system.time(mh(100,dtnorm_mix2,c(-4,4),FALSE))[3]
mhs62<-system.time(mh(10000,dtnorm_mix2,c(-4,4),FALSE))[3]
mhs63<-system.time(mh(1000000,dtnorm_mix2,c(-4,4),FALSE))[3]
mhs64<-system.time(mh(10000000,dtnorm_mix2,c(-4,4),FALSE))[3]

mhm11<-system.time(mh(100,dbetann,c(0,1),TRUE))[3]
mhm12<-system.time(mh(10000,dbetann,c(0,1),TRUE))[3]
mhm13<-system.time(mh(1000000,dbetann,c(0,1),TRUE))[3]
mhm14<-system.time(mh(10000000,dbetann,c(0,1),TRUE))[3]

mhm21<-system.time(mh(100,dtnorm,c(-3,3),TRUE))[3]
mhm22<-system.time(mh(10000,dtnorm,c(-3,3),TRUE))[3]
mhm23<-system.time(mh(1000000,dtnorm,c(-3,3),TRUE))[3]
mhm24<-system.time(mh(10000000,dtnorm,c(-3,3),TRUE))[3]

mhm31<-system.time(mh(100,dtexp,c(0,4),TRUE))[3]
mhm32<-system.time(mh(10000,dtexp,c(0,4),TRUE))[3]
mhm33<-system.time(mh(1000000,dtexp,c(0,4),TRUE))[3]
mhm34<-system.time(mh(10000000,dtexp,c(0,4),TRUE))[3]

mhm41<-system.time(mh(100,dunif_mix,c(-3,4),TRUE))[3]
mhm42<-system.time(mh(10000,dunif_mix,c(-3,4),TRUE))[3]
mhm43<-system.time(mh(1000000,dunif_mix,c(-3,4),TRUE))[3]
mhm44<-system.time(mh(10000000,dunif_mix,c(-3,4),TRUE))[3]

mhm51<-system.time(mh(100,dtnorm_mix1,c(0,10),TRUE))[3]
mhm52<-system.time(mh(10000,dtnorm_mix1,c(0,10),TRUE))[3]
mhm53<-system.time(mh(1000000,dtnorm_mix1,c(0,10),TRUE))[3]
mhm54<-system.time(mh(10000000,dtnorm_mix1,c(0,10),TRUE))[3]

mhm61<-system.time(mh(100,dtnorm_mix2,c(-4,4),TRUE))[3]
mhm62<-system.time(mh(10000,dtnorm_mix2,c(-4,4),TRUE))[3]
mhm63<-system.time(mh(1000000,dtnorm_mix2,c(-4,4),TRUE))[3]
mhm64<-system.time(mh(10000000,dtnorm_mix2,c(-4,4),TRUE))[3]

r1=cbind(mhs11,mhs12,mhs13,mhs14,mhm11,mhm12,mhm13,mhm14)
r2=cbind(mhs21,mhs22,mhs23,mhs24,mhm21,mhm22,mhm23,mhm24)
r3=cbind(mhs31,mhs32,mhs33,mhs34,mhm31,mhm32,mhm33,mhm34)
r4=cbind(mhs41,mhs42,mhs43,mhs44,mhm41,mhm42,mhm43,mhm44)
r5=cbind(mhs51,mhs52,mhs53,mhs54,mhm51,mhm52,mhm53,mhm54)
r6=cbind(mhs61,mhs62,mhs63,mhs64,mhm61,mhm62,mhm63,mhm64)

t=rbind(r1,r2,r3,r4,r5,r6)

options(warn=-1)
table=xtable(t,type="html",digits=3)
options(warn=0)

