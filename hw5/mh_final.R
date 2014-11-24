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

#score function
score = function(x, dfunc) 
{
  stopifnot(is.numeric(x) & length(x))
  
  x = sort(x)
  n = length(x)
  
  ex = ecdf(x)(x)
  
  dx = dfunc(x)
  ed = cumsum(c(0, (x[-1]-x[-n])*(dx[-1]+dx[-n])/2))
  
  #hist(x, breaks = 1000,freq = FALSE)
  #lines(x,dfunc(x))
  
  return( sqrt(sum((ex-ed)^2)/n) )
}

## test samples
#beta 0.9, 0.9
dbetann = function(x)
{
  dbeta(x,0.9,0.9)
}

#truncated normal
dtnorm = function(x)
{
  ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)
}

#truncated exponential
dtexp = function(x)
{
  ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)
}

#uniform mixture
dunif_mix = function(x)
{
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))
}

#truncated normal mixture 1
dtnorm_mix1 = function(x)
{
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.9206407)
}

#truncated normal mixture 2
dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.55)
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

r1=cbind(mhs11/n1,mhs12/n2,mhs13/n3,mhs14/n4,mhm11/n1,mhm12/n2,mhm13/n3,mhm14/n4)
r2=cbind(mhs21/n1,mhs22/n2,mhs23/n3,mhs24/n4,mhm21/n1,mhm22/n2,mhm23/n3,mhm24/n4)
r3=cbind(mhs31/n1,mhs32/n2,mhs33/n3,mhs34/n4,mhm31/n1,mhm32/n2,mhm33/n3,mhm34/n4)
r4=cbind(mhs41/n1,mhs42/n2,mhs43/n3,mhs44/n4,mhm41/n1,mhm42/n2,mhm43/n3,mhm44/n4)
r5=cbind(mhs51/n1,mhs52/n2,mhs53/n3,mhs54/n4,mhm51/n1,mhm52/n2,mhm53/n3,mhm54/n4)
r6=cbind(mhs61/n1,mhs62/n2,mhs63/n3,mhs64/n4,mhm61/n1,mhm62/n2,mhm63/n3,mhm64/n4)
r7=cbind("single_100","single_10000","single_1000000","single_10000000","multiple_100","multiple_10000","multiple_1000000","multiple_10000000")

options(warn=-1)
t=data.frame(rbind(r7,r1,r2,r3,r4,r5,r6))
options(warn=0)
label=data.frame(rbind("MH_sampler","dbetann","dtnorm","dtexp","dunif_mix","dtnorm_mix1","dtnorm_mix2"))

sc1=score(mh(1000000,dbetann,c(0,1),FALSE),dbetann)
sc2=score(mh(1000000,dtnorm,c(-3,3),FALSE),dtnorm)
sc3=score(mh(1000000,dtexp,c(0,6),FALSE),dtexp)
sc4=score(mh(1000000,dunif_mix,c(-3,4),FALSE),dunif_mix)
sc5=score(mh(1000000,dtnorm_mix1,c(0,10),FALSE),dtnorm_mix1)
sc6=score(mh(1000000,dtnorm_mix2,c(-4,4),FALSE),dtnorm_mix2)

mc1=score(mh(1000000,dbetann,c(0,1),TRUE),dbetann)
mc2=score(mh(1000000,dtnorm,c(-3,3),TRUE),dtnorm)
mc3=score(mh(1000000,dtexp,c(0,6),TRUE),dtexp)
mc4=score(mh(1000000,dunif_mix,c(-3,4),TRUE),dunif_mix)
mc5=score(mh(1000000,dtnorm_mix1,c(0,10),TRUE),dtnorm_mix1)
mc6=score(mh(1000000,dtnorm_mix2,c(-4,4),TRUE),dtnorm_mix2)

sc=data.frame(rbind("SCscore",sc1,sc2,sc3,sc4,sc5,sc6))
mc=data.frame(rbind("MCscore",mc1,mc2,mc3,mc4,mc5,mc6))

mainmh=data.frame(cbind(label,t,sc,mc))

save(mainmh, file="MHtimes.Rdata")

