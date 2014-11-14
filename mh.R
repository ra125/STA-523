#Add factors to increase and decrease acc prob

mh = function(n, dfunc, range, mc.cores=FALSE){ 
  
stopifnot(is.function(dfunc) & is.numeric(n))

x=runif(1,range[1],range[2])
vec=rep(0,n)
dvec=rep(0,n)
vec[1]=x
dvec[1]=dfunc(x)

for(i in 2:n)
{
  sample=rnorm(1,x,2)
  
  if(sample>range[1] & sample<range[2]){
  acc=dfunc(sample)/dfunc(x)
  
  vec[i]=x
  dvec[i]=dfunc(x)
  if(acc>runif(1,0,1))
  {
    x=sample
    vec[i]=x
    dvec[i]=dfunc(x)
  }
  }
}

p=plot(vec,dvec)

return(p)
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

mh(n=10000, dfunc=dbetann, range=c(0,1), mc.cores=FALSE)
mh(n=10000, dfunc=dtnorm, range=c(-3,3), mc.cores=FALSE)
mh(n=10000, dfunc=dtexp, range=c(0,6), mc.cores=FALSE)
mh(n=10000, dfunc=dunif_mix, range=c(-3,4), mc.cores=FALSE)
mh(n=10000, dfunc=dtnorm_mix1, range=c(0,10), mc.cores=FALSE)
mh(n=10000, dfunc=dtnorm_mix2, range=c(-4,4), mc.cores=FALSE)
