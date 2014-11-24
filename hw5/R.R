### R function 
library(xtable)
source("check_packages.R")
check_packages(c("parallel","xtable", "doMC", "foreach", "truncnorm", "iterators")) 

R = function(n,dfunc,range,mc){
  
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
  
  if(as.character(substitute(dfunc))=="dbetann"){
    return(unlist(mclapply(1:mc,function(x) rbeta(ceiling(n/mc),0.9,0.9),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtnorm"){
    return(unlist(mclapply(1:mc,function(x) rtruncnorm(ceiling(n/mc),-3,3),
                           mc.cores=mc)))}
  
  if(as.character(substitute(dfunc))=="dtexp"){
    sample=unlist(mclapply(1:mc, function(x) rexp(ceiling((n/mc)*1.02),rate=1/3),
                   mc.cores=mc))
    return(sample[sample<6])
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

Rs11<-system.time(R(100,dbetann,c(0,1),FALSE))[3]
Rs12<-system.time(R(10000,dbetann,c(0,1),FALSE))[3]
Rs13<-system.time(R(1000000,dbetann,c(0,1),FALSE))[3]
Rs14<-system.time(R(10000000,dbetann,c(0,1),FALSE))[3]

Rs21<-system.time(R(100,dtnorm,c(-3,3),FALSE))[3]
Rs22<-system.time(R(10000,dtnorm,c(-3,3),FALSE))[3]
Rs23<-system.time(R(1000000,dtnorm,c(-3,3),FALSE))[3]
Rs24<-system.time(R(10000000,dtnorm,c(-3,3),FALSE))[3]

Rs31<-system.time(R(100,dtexp,c(0,4),FALSE))[3]
Rs32<-system.time(R(10000,dtexp,c(0,4),FALSE))[3]
Rs33<-system.time(R(1000000,dtexp,c(0,4),FALSE))[3]
Rs34<-system.time(R(10000000,dtexp,c(0,4),FALSE))[3]

Rs41<-system.time(R(100,dunif_mix,c(-3,4),FALSE))[3]
Rs42<-system.time(R(10000,dunif_mix,c(-3,4),FALSE))[3]
Rs43<-system.time(R(1000000,dunif_mix,c(-3,4),FALSE))[3]
Rs44<-system.time(R(10000000,dunif_mix,c(-3,4),FALSE))[3]

Rs51<-system.time(R(100,dtnorm_mix1,c(0,10),FALSE))[3]
Rs52<-system.time(R(10000,dtnorm_mix1,c(0,10),FALSE))[3]
Rs53<-system.time(R(1000000,dtnorm_mix1,c(0,10),FALSE))[3]
Rs54<-system.time(R(10000000,dtnorm_mix1,c(0,10),FALSE))[3]

Rs61<-system.time(R(100,dtnorm_mix2,c(-4,4),FALSE))[3]
Rs62<-system.time(R(10000,dtnorm_mix2,c(-4,4),FALSE))[3]
Rs63<-system.time(R(1000000,dtnorm_mix2,c(-4,4),FALSE))[3]
Rs64<-system.time(R(10000000,dtnorm_mix2,c(-4,4),FALSE))[3]

Rm11<-system.time(R(100,dbetann,c(0,1),TRUE))[3]
Rm12<-system.time(R(10000,dbetann,c(0,1),TRUE))[3]
Rm13<-system.time(R(1000000,dbetann,c(0,1),TRUE))[3]
Rm14<-system.time(R(10000000,dbetann,c(0,1),TRUE))[3]

Rm21<-system.time(R(100,dtnorm,c(-3,3),TRUE))[3]
Rm22<-system.time(R(10000,dtnorm,c(-3,3),TRUE))[3]
Rm23<-system.time(R(1000000,dtnorm,c(-3,3),TRUE))[3]
Rm24<-system.time(R(10000000,dtnorm,c(-3,3),TRUE))[3]

Rm31<-system.time(R(100,dtexp,c(0,4),TRUE))[3]
Rm32<-system.time(R(10000,dtexp,c(0,4),TRUE))[3]
Rm33<-system.time(R(1000000,dtexp,c(0,4),TRUE))[3]
Rm34<-system.time(R(10000000,dtexp,c(0,4),TRUE))[3]

Rm41<-system.time(R(100,dunif_mix,c(-3,4),TRUE))[3]
Rm42<-system.time(R(10000,dunif_mix,c(-3,4),TRUE))[3]
Rm43<-system.time(R(1000000,dunif_mix,c(-3,4),TRUE))[3]
Rm44<-system.time(R(10000000,dunif_mix,c(-3,4),TRUE))[3]

Rm51<-system.time(R(100,dtnorm_mix1,c(0,10),TRUE))[3]
Rm52<-system.time(R(10000,dtnorm_mix1,c(0,10),TRUE))[3]
Rm53<-system.time(R(1000000,dtnorm_mix1,c(0,10),TRUE))[3]
Rm54<-system.time(R(10000000,dtnorm_mix1,c(0,10),TRUE))[3]

Rm61<-system.time(R(100,dtnorm_mix2,c(-4,4),TRUE))[3]
Rm62<-system.time(R(10000,dtnorm_mix2,c(-4,4),TRUE))[3]
Rm63<-system.time(R(1000000,dtnorm_mix2,c(-4,4),TRUE))[3]
Rm64<-system.time(R(10000000,dtnorm_mix2,c(-4,4),TRUE))[3]

n1=100
n2=10000
n3=1000000
n4=10000000

r1=cbind(Rs11/n1,Rs12/n2,Rs13/n3,Rs14/n4,Rm11/n1,Rm12/n2,Rm13/n3,Rm14/n4)
r2=cbind(Rs21/n1,Rs22/n2,Rs23/n3,Rs24/n4,Rm21/n1,Rm22/n2,Rm23/n3,Rm24/n4)
r3=cbind(Rs31/n1,Rs32/n2,Rs33/n3,Rs34/n4,Rm31/n1,Rm32/n2,Rm33/n3,Rm34/n4)
r4=cbind(Rs41/n1,Rs42/n2,Rs43/n3,Rs44/n4,Rm41/n1,Rm42/n2,Rm43/n3,Rm44/n4)
r5=cbind(Rs51/n1,Rs52/n2,Rs53/n3,Rs54/n4,Rm51/n1,Rm52/n2,Rm53/n3,Rm54/n4)
r6=cbind(Rs61/n1,Rs62/n2,Rs63/n3,Rs64/n4,Rm61/n1,Rm62/n2,Rm63/n3,Rm64/n4)
r7=cbind("single_100","single_10000","single_1000000","single_10000000","multiple_100","multiple_10000","multiple_1000000","multiple_10000000")

options(warn=-1)
t=data.frame(rbind(r7,r1,r2,r3,r4,r5,r6))
options(warn=0)
label=data.frame(rbind("R_sampler","dbetann","dtnorm","dtexp","dunif_mix","dtnorm_mix1","dtnorm_mix2"))

sc1=score(R(1000000,dbetann,c(0,1),FALSE),dbetann)
sc2=score(R(1000000,dtnorm,c(-3,3),FALSE),dtnorm)
sc3=score(R(1000000,dtexp,c(0,6),FALSE),dtexp)
sc4=score(R(1000000,dunif_mix,c(-3,4),FALSE),dunif_mix)
sc5=score(R(1000000,dtnorm_mix1,c(0,10),FALSE),dtnorm_mix1)
sc6=score(R(1000000,dtnorm_mix2,c(-4,4),FALSE),dtnorm_mix2)

mc1=score(R(1000000,dbetann,c(0,1),TRUE),dbetann)
mc2=score(R(1000000,dtnorm,c(-3,3),TRUE),dtnorm)
mc3=score(R(1000000,dtexp,c(0,6),TRUE),dtexp)
mc4=score(R(1000000,dunif_mix,c(-3,4),TRUE),dunif_mix)
mc5=score(R(1000000,dtnorm_mix1,c(0,10),TRUE),dtnorm_mix1)
mc6=score(R(1000000,dtnorm_mix2,c(-4,4),TRUE),dtnorm_mix2)

sc=data.frame(rbind("SCscore",sc1,sc2,sc3,sc4,sc5,sc6))
mc=data.frame(rbind("MCscore",mc1,mc2,mc3,mc4,mc5,mc6))

mainr=data.frame(cbind(label,t,sc,mc))

save(mainr, file="Rtimes.Rdata")
