### R function 
library(xtable)

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

r1=cbind(Rs11,Rs12,Rs13,Rs14,Rm11,Rm12,Rm13,Rm14)
r2=cbind(Rs21,Rs22,Rs23,Rs24,Rm21,Rm22,Rm23,Rm24)
r3=cbind(Rs31,Rs32,Rs33,Rs34,Rm31,Rm32,Rm33,Rm34)
r4=cbind(Rs41,Rs42,Rs43,Rs44,Rm41,Rm42,Rm43,Rm44)
r5=cbind(Rs51,Rs52,Rs53,Rs54,Rm51,Rm52,Rm53,Rm54)
r6=cbind(Rs61,Rs62,Rs63,Rs64,Rm61,Rm62,Rm63,Rm64)

t=rbind(r1,r2,r3,r4,r5,r6)

options(warn=-1)
table=xtable(t,type="html")
options(warn=0)
