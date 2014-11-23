
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
  
}  # end of function: reject






######## NOW TESTING





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












###### Scoring table

s1=100
s2=10000
s3=1000000
s4=10000000


reject_score_dbetann_sc<-score(reject(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE), dbetann)
reject_score_dbetann_mc<-score(reject(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE), dbetann)

reject_score_dtnorm_sc<-score(reject(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE),dtnorm)
reject_score_dtnorm_mc<-score(reject(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE),dtnorm)

reject_score_dtexp_sc<-score(reject(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE),dtexp)
reject_score_dtexp_mc<-score(reject(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE),dtexp)

reject_score_dunif_mix_sc<-score(reject(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE),dunif_mix)
reject_score_dunif_mix_mc<-score(reject(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE),dunif_mix)

reject_score_dtnorm_mix1_sc<-score(reject(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE),dtnorm_mix1)
reject_score_dtnorm_mix1_mc<-score(reject(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE),dtnorm_mix1)

reject_score_dtnorm_mix2_sc<-score(reject(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE),dtnorm_mix2)
reject_score_dtnorm_mix2_mc<-score(reject(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE),dtnorm_mix2)

score_reject_names=c("reject_score_dbetann_sc","reject_score_dbetann_mc","reject_score_dtnorm_sc","reject_score_dtnorm_mc",
                     "reject_score_dtexp_sc","reject_score_dtexp_mc","reject_score_dunif_mix_sc","reject_score_dunif_mix_mc",
                     "reject_score_dtnorm_mix1_sc","reject_score_dtnorm_mix1_mc","reject_score_dtnorm_mix2_sc","reject_score_dtnorm_mix2_mc")

score_rejec=c(reject_score_dbetann_sc,reject_score_dbetann_mc,reject_score_dtnorm_sc,reject_score_dtnorm_mc,
              reject_score_dtexp_sc,reject_score_dtexp_mc,reject_score_dunif_mix_sc,reject_score_dunif_mix_mc,
              reject_score_dtnorm_mix1_sc,reject_score_dtnorm_mix1_mc,reject_score_dtnorm_mix2_sc,reject_score_dtnorm_mix2_mc)

score_reject_data=data.frame(cbind(score_reject_names, score_reject))

# save as a dataframe

save(score_reject_data, file="score_reject_data.Rdata")



###### recording time --- SINGLE CORE

reject_dbetann=rbind(
  system.time(reject(n=s1, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dbetann, range=c(0,1), mc=FALSE))
)

reject_dtnorm=rbind(
  system.time(reject(n=s1, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dtnorm, range=c(-3,3), mc=FALSE))
)

reject_dtexp=rbind(
  system.time(reject(n=s1, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dtexp, range=c(0,6), mc=FALSE))
)

reject_dunif_mix=rbind(
  system.time(reject(n=s1, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dunif_mix, range=c(-3,4), mc=FALSE))
)

reject_dtnorm_mix1=rbind(
  system.time(reject(n=s1, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE))
)

reject_dtnorm_mix2=rbind(
  system.time(reject(n=s1, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(reject(n=s2, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(reject(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(reject(n=s4, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE))
)


reject_single=c(
  "reject_dbetann",
  "reject_dtnorm",
  "reject_dtexp",
  "reject_dunif_mix",
  "reject_dunif_mix",
  "reject_dtnorm_mix1",
  "reject_dtnorm_mix2"
)

s=c(100, 
    10000,
    1000000,
    10000000)

names=c(rep(reject_single[1],4), rep(reject_single[2],4), rep(reject_single[3],4),
        rep(reject_single[4],4),rep(reject_single[5],4),rep(reject_single[6],4),
        rep(reject_single[7],4)  )


iteration=rep(s,7)

reject_all=c(reject_dbetann[,3], reject_dtnorm[,3],
             reject_dtexp[,3], reject_dunif_mix[,3], reject_dunif_mix[,3],
             reject_dtnorm_mix1[,3], reject_dtnorm_mix2[,3])

reject_data=data.frame(cbind(names, iteration, "time"=reject_all) )

## save as a dataframe

save(reject_data, file="reject_data.Rdata")



######  recording time --- MULTI CORE (rejection sampler)

reject_dbetann_T=rbind(
  system.time(reject(n=s1, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(reject(n=s2, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(reject(n=s3, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(reject(n=s4, dfunc=dbetann, range=c(0,1), mc=TRUE))
)

reject_dtnorm_T=rbind(
  system.time(reject(n=s1, dfunc=dtnorm, range=c(-3,3), mc=TRUE),
              system.time(reject(n=s2, dfunc=dtnorm, range=c(-3,3), mc=TRUE)),
              system.time(reject(n=s3, dfunc=dtnorm, range=c(-3,3), mc=TRUE)),
              system.time(reject(n=s4, dfunc=dtnorm, range=c(-3,3), mc=TRUE))
  )
  
  reject_dtexp_T=rbind(
    system.time(reject(n=s1, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(reject(n=s2, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(reject(n=s3, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(reject(n=s4, dfunc=dtexp, range=c(0,6), mc=TRUE))
  )
  
  reject_dunif_mix_T=rbind(
    system.time(reject(n=s1, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(reject(n=s2, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(reject(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(reject(n=s4, dfunc=dunif_mix, range=c(-3,4), mc=TRUE))
  )
  
  reject_dtnorm_mix1_T=rbind(
    system.time(reject(n=s1, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(reject(n=s2, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(reject(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(reject(n=s4, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE))
  )
  
  reject_dtnorm_mix2_T=rbind(
    system.time(reject(n=s1, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(reject(n=s2, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(reject(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(reject(n=s4, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE))
  )
  
  
  reject_single_T=c(
    "reject_dbetann_T",
    "reject_dtnorm_T",
    "reject_dtexp_T",
    "reject_dunif_mix_T",
    "reject_dunif_mix_T",
    "reject_dtnorm_mix1_T",
    "reject_dtnorm_mix2_T"
  )
  
  s=c(100, 
      10000,
      1000000,
      10000000)
  
  names=c(rep(reject_single_T[1],4), rep(reject_single_T[2],4), rep(reject_single_T[3],4),
          rep(reject_single_T[4],4),rep(reject_single_T[5],4),rep(reject_single_T[6],4),
          rep(reject_single_T[7],4)  )
  
  
  iteration=rep(s,7)
  
  
  reject_all_T=c(reject_dbetann_T[,3], reject_dtnorm_T[,3],
                 reject_dtexp_T[,3], reject_dunif_mix_T[,3], reject_dunif_mix_T[,3],
                 reject_dtnorm_mix1_T[,3], reject_dtnorm_mix2_T[,3])
  
  reject_data_T=data.frame(cbind(names, iteration, "time"=reject_all_T) )
  
  ## save as a dataframe
  
  save(score_reject_data_T, file="score_reject_data.Rdata")
  
 
