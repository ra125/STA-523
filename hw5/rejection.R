
#Rejection sampler
#strategy
#1. sample uniformly from the X axis

#2. Reject value if it's over the value


reject = function(n, dfunc, range, mc.cores=FALSE){ 

stopifnot(is.function(dfunc) & is.numeric(n))

## 1) generate values of Y from uniform
axes=runif(n, min(range), max(range))

sample=runif(n, min(axes), max(axes))
ys<-dfunc(x=sample)
sample<-runif(n, min=min(ys), max=max(ys))

## 2) reject if Y is larger than the value from dfunc

vector=NULL
for(i in 1:length(axes)){
if(sample[i]<dfunc(axes[i]) ){
vector<-c(vector, sample[i])  
}
print(vector)
}


# hist(vector)
# hist(ys)
# 

# vector<-dfunc(x=sample)
# binary<-rbinom(n=n, size=1, prob=0.5)
# final<-vector[!binary==0]
# 
# p<-plot(sample[!binary==0],final)

# ### Partitioner --- 
# nm = names(map_res) %>% unique() %>% sort()
# part_res = lapply(nm, function(x) unlist(map_res[names(map_res)==x])) %>% 
#   setNames(nm)

# return(summary(final))
# return(p)
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
