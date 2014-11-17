slice = function(n, dfunc, range, mc=FALSE)
{
  rangec=range
  res<-matrix(NA,n,ncol=1,nrow=n)
  w=(range[1]-range[2])/1000
  for (i in 1:n)
  {
    res[i]=runif(1,rangec[1],rangec[2])
    y=runif(1,0,dfunc(res[i]))
    rangec=rep(y,2)
    while (dfunc(rangec[1])>y)
    {
      rangec[1]=rangec[1]-w
    }
    while (dfunc(rangec[2])>y)
    {
      rangec[2]=rangec[2]+w
    }
    rangec[1]=rangec[1]+rbinom(1,1,0.5)*w
    rangec[2]=rangec[1]-rbinom(1,1,0.5)*w
  }
  return
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
           +0.5*dnorm(x,mean=6,sd=1))/0.90059152)
}

#truncated normal mixture 2
dtnorm_mix2 = function(x)
{
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.4999683)
}



reject(n=100000, dfunc=dbetann, range=c(0,1), mc.cores=FALSE)
reject(n=10000, dfunc=dtnorm, range=c(-3,3), mc.cores=FALSE)
reject(n=10000, dfunc=dtexp, range=c(0,6), mc.cores=FALSE)
reject(n=10000, dfunc=dunif_mix, range=c(-3,4), mc.cores=FALSE)
reject(n=10000, dfunc=dtnorm_mix1, range=c(0,10), mc.cores=FALSE)


reject(n=10000, dfunc=dtnorm_mix2, range=c(-4,4), mc.cores=FALSE)



##scoring

score = function(x, dfunc) 
{
  stopifnot(is.numeric(x) & length(x))
  
  x = sort(x)
  n = length(x)
  
  ex = ecdf(x)(x)
  
  dx = dfunc(x)
  ed = cumsum(c(0, (x[-1]-x[-n])*(dx[-1]+dx[-n])/2))
  
  return( sqrt(sum((ex-ed)^2)/n) )
}

score(1000, dbetann)