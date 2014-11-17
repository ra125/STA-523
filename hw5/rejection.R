
#Rejection sampler
#strategy
#1. sample uniformly from the X axis -- this is the envelop density. Since each test distribution functions specified
# ranges, the envelop function is Unifrom (min(range), max(range))
#2. using the max and min values of the density function, sample Ys from uniform as well.
#3. Throw away the Y value if it's over the value drawn from the density function and keep the rest.



reject = function(n, dfunc, range, mc=FALSE){ 
stopifnot(is.function(dfunc) & is.numeric(n))

## 1) generate values of Xs and Ys from uniform
xs=runif(n, min(range), max(range))
ys<-dfunc(x=xs)
sample<-runif(n, min=min(ys), max=max(ys))


## 2) reject if Y is larger than the value from dfunc
vector=NULL
for(i in 1:length(xs)){
        if( sample[i]<ys[i] ) {
        vector<-c(vector, sample[i])  
                              }
                      }
return(vector)
#hist(as.vector(vector))  # for testing purpose


# ### Partitioner ###
# if(mc=TRUE) {
# nm = names(map_res) %>% unique() %>% sort()
# part_res = lapply(nm, function(x) unlist(map_res[names(map_res)==x])) %>% 
#   setNames(nm)
# }
  
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



reject(n=10000, dfunc=dbetann, range=c(0,1), mc=FALSE)
reject(n=10000, dfunc=dtnorm, range=c(-3,3), mc=FALSE)
reject(n=10000, dfunc=dtexp, range=c(0,6), mc=FALSE)
reject(n=10000, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)
reject(n=10000, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)
reject(n=10000, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)



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



score(reject(n=100000, dfunc=dbetann, range=c(0,1), mc=FALSE), dbetann)
score(reject(n=100000, dfunc=dtnorm, range=c(-3,3), mc=FALSE),dtnorm)
score(reject(n=100000, dfunc=dtexp, range=c(0,6), mc=FALSE),dtexp)
score(reject(n=100000, dfunc=dunif_mix, range=c(-3,4), mc=FALSE),dunif_mix)
score(reject(n=100000, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE),dtnorm_mix1)
score(reject(n=100000, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE),dtnorm_mix2)

