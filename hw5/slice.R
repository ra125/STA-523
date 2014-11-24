slice = function(n, dfunc, range, mc)
{
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
  
  check<-function(x)
  {
    if (is.finite(x) & is.numeric(x)) return(0)
    else
    {
      if (is.na(x)==TRUE) return(1)
      else
      {
        if (is.nan(x)==TRUE) return(2)
        else 
        {
          if ((is.infinite(x)==TRUE)) return(3)
          else return(4)
        }
      }
    }
  }
  
  sampling<-function(nc,dfunc,range,cores) 
  {
    rangec=range
    res<-matrix(NA,n,ncol=1,nrow=nc)
    w=abs(rangec[1]-rangec[2])/10
    y=0
    for (i in 1:nc)
    {
      res[i]=runif(1,rangec[1],rangec[2])
#       stopifnot(check(res[i])==0)
#       stopifnot(check(dfunc(res[i]))==0)
      k=0
      kk=0
      while (dfunc(res[i])<y) 
      {
        if (res[i]<res[i-1])
        {
          rangec[1]=res[i]
        }
        else
        {
          if (res[i]>res[i-1])
          {
            rangec[2]=res[i]
          }
        }
        res[i]=runif(1,rangec[1],rangec[2])
#         stopifnot(check(res[i])==0)
#         stopifnot(check(dfunc(res[i]))==0)
        k=k+1
        if(k>10)
        {
          kk=kk+1

#           print(c(k,kk,res[i],cores))
=======
  #        print(c(k,kk,res[i],cores))

          rangec=range
          res[i]=runif(1,rangec[1],rangec[2])
#           stopifnot(check(res[i])==0)
#           stopifnot(check(dfunc(res[i]))==0)
          break
        }
      }
      y=runif(1,0,dfunc(res[i]))
      rangec=rep(res[i],2)
      while (dfunc(rangec[1])>y)
      {
        rangec[1]=rangec[1]-w
      }
      while (dfunc(rangec[2])>y)
      {
        rangec[2]=rangec[2]+w
      }
    }
    return(res)
  }
  
  cores=mc
  nc=ceiling(n/cores)
  sample<-unlist(mclapply(1:cores, function(z) sampling(nc,dfunc,range,cores),mc.cores = cores))
  sample=sample[1:n]
  return(sample)
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

#dbetann
(tdbetann<-sapply(c(100,10000,1000000,10000000),
       function(x)
       {
          sapply(c(FALSE,TRUE),
                 function(y) 
                   {
#                    print(c(x,y))
                   tperit<-system.time(sample<-slice(x,dbetann,c(0,1),mc = y))[3]/x
                   ttotal<-tperit*x
                   sc=score(sample,dbetann)
                   return(c(ttotal,tperit,sc))
                 })
       }))

(tdtnorm<-sapply(c(100,10000,1000000,10000000),
                  function(x)
                  {
                    sapply(c(FALSE,TRUE),
                           function(y) 
                           {
#                              print(c(x,y))
                             tperit<-system.time(sample<-slice(x,dtnorm,c(-3,3),mc = y))[3]/x
                             ttotal<-tperit*x
                             sc=score(sample,dtnorm)
                             return(c(ttotal,tperit,sc))
                           })
                  }))

(tdtexp<-sapply(c(100,10000,1000000,10000000),
                 function(x)
                 {
                   sapply(c(FALSE,TRUE),
                          function(y) 
                          {
#                             print(c(x,y))
                            tperit<-system.time(sample<-slice(x,dtexp,c(0,6),mc = y))[3]/x
                            ttotal<-tperit*x
                            sc=score(sample,dtexp)
                            return(c(ttotal,tperit,sc))
                          })
                 }))

(tdunif_mix<-sapply(c(100,10000,1000000,10000000),
                function(x)
                {
                  sapply(c(FALSE,TRUE),
                         function(y) 
                         {
#                            print(c(x,y))
                           tperit<-system.time(sample<-slice(x,dunif_mix,c(-3,4),mc = y))[3]/x
                           ttotal<-tperit*x
                           sc=score(sample,dunif_mix)
                           return(c(ttotal,tperit,sc))
                         })
                }))

(tdtnorm_mix1<-sapply(c(100,10000,1000000,10000000),
                    function(x)
                    {
                      sapply(c(FALSE,TRUE),
                             function(y) 
                             {
#                                print(c(x,y))
                               tperit<-system.time(sample<-slice(x,dtnorm_mix1,c(0,10),mc = y))[3]/x
                               ttotal<-tperit*x
                               sc=score(sample,dtnorm_mix1)
                               return(c(ttotal,tperit,sc))
                             })
                    }))

(tdtnorm_mix2<-sapply(c(100,10000,1000000,10000000),
                      function(x)
                      {
                        sapply(c(FALSE,TRUE),
                               function(y) 
                               {
#                                  print(c(x,y))
                                 tperit<-system.time(sample<-slice(x,dtnorm_mix2,c(-4,4),mc = y))[3]/x
                                 ttotal<-tperit*x
                                 sc=score(sample,dtnorm_mix2)
                                 return(c(ttotal,tperit,sc))
                               })
                      }))

slice_data_T<-as.data.frame(matrix(NA,ncol=6,nrow=24))
name<-rep(c("slice_dbetann","slice_dtnorm","slice_texp","slice_dunif_mix","slice_dtnorm_mix1","slice_dtnorm_mix2"),each=4)
times<-rep(c(100,10000,1000000,10000000),6)
slice_data_T[,1]=name
slice_data_T[,2]=times
names(slice_data_T)=c("names","times","single core total time","single core score","multi-core total time","multi-core score")
slice_data_T[1:4,3:6]=t(tdbetann[c(1,3,4,6),])
slice_data_T[5:8,3:6]=t(tdtnorm[c(1,3,4,6),])
slice_data_T[9:12,3:6]=t(tdtexp[c(1,3,4,6),])
slice_data_T[13:16,3:6]=t(tdunif_mix[c(1,3,4,6),])
slice_data_T[17:20,3:6]=t(tdtnorm_mix1[c(1,3,4,6),])
slice_data_T[21:24,3:6]=t(tdtnorm_mix2[c(1,3,4,6),])


save(slice_data_T, file="slice_data_T.Rdata")

#dtnorm
# sapply(c(100,10000,1000000,10000000),
#        function(x)
#        {
#          sapply(1:8,
#                 function(y)
#                 {
#                   #                    if (mc==TRUE && n>1000){
#                   cores = y
#                   return(system.time(unlist(mclapply(1:cores, function(z) slice(ceiling(x/cores), dtnorm, range=c(-3,3), TRUE),
#                                                      mc.cores = cores) ))[3]/x)
#                   #                    }else{
#                   #                      return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
#                   #                    }
#                 }) 
#        })
# 
# #dtexp
# sapply(c(100,10000,1000000,10000000),
#        function(x)
#        {
#          sapply(1:8,
#                 function(y)
#                 {
#                   #                    if (mc==TRUE && n>1000){
#                   cores = y
#                   return(system.time(unlist(mclapply(1:cores, function(z) slice(ceiling(x/cores), dtexp, range=c(0,6), TRUE),
#                                                      mc.cores = cores) ))[3]/x)
#                   #                    }else{
#                   #                      return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
#                   #                    }
#                 }) 
#        })
# 
# #dunif_mix
# sapply(c(100,10000,1000000,10000000),
#        function(x)
#        {
#          sapply(1:8,
#                 function(y)
#                 {
#                   #                    if (mc==TRUE && n>1000){
#                   cores = y
#                   return(system.time(unlist(mclapply(1:cores, function(z) slice(ceiling(x/cores), dunif_mix, range=c(-3,4), TRUE),
#                                                      mc.cores = cores) ))[3]/x)
#                   #                    }else{
#                   #                      return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
#                   #                    }
#                 }) 
#        })
# 
# #dtnorm_mix1
# sapply(c(100,10000,1000000,10000000),
#        function(x)
#        {
#          sapply(c(4,8),
#                 function(y)
#                 {
#                   #                    if (mc==TRUE && n>1000){
#                   cores = y
#                   return(system.time(unlist(mclapply(1:cores, function(z) slice(ceiling(x/cores), dtnorm_mix1, range=c(0,10), TRUE),
#                                                      mc.cores = cores) ))[3]/x)
#                   #                    }else{
#                   #                      return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
#                   #                    }
#                 }) 
#        })
# 
# #dtnorm_mix2
# sapply(c(100,10000,1000000,10000000),
#        function(x)
#        {
#          sapply(c(1,8),
#                 function(y)
#                 {
#                   #                    if (mc==TRUE && n>1000){
#                   cores = y
#                   return(system.time(slice())[3]/x)
#                   #                    }else{
#                   #                      return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
#                   #                    }
#                 }) 
#        })

# if (mc==TRUE && n>1000){
#   cores = y
#   return(system.time(unlist(mclapply(1:cores, function(x) slice(ceiling(x/cores), dfunc, range, TRUE),
#                                      mc.cores = cores) ))[3]/x)
# }else{
#   return(system.time(slice(x, dfunc, range, TRUE)[3])/x)
# }

nofr=10000

system.time(edbetann<-slice(n=nofr, dfunc=dbetann, range=c(0,1), mc=FALSE)) 
system.time(edtnorm<-slice(n=nofr, dfunc=dtnorm, range=c(-3,3), mc=FALSE))
system.time(edtexp<-slice(n=nofr, dfunc=dtexp, range=c(0,6), mc=FALSE))
system.time(edunif_mix<-slice(n=nofr, dfunc=dunif_mix, range=c(-3,4), mc=FALSE))
system.time(edtnorm_mix1<-slice(n=nofr, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE))
system.time(edtnorm_mix2<-slice(n=nofr, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE))

##scoring


score(edbetann, dbetann)
score(edtnorm,dtnorm)
score(edtexp,dtexp)
score(edunif_mix,dunif_mix)
score(edtnorm_mix1,dtnorm_mix1)
score(edtnorm_mix2,dtnorm_mix2)

flist=c(dbetann,dtnorm)




################ 
################ Creating tables!!!
################ 

###### Scoring table

s1=100
s2=10000
s3=1000000
s4=10000000


slice_score_dbetann_sc<-score(slice(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE), dbetann)
slice_score_dbetann_mc<-score(slice(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE), dbetann)

slice_score_dtnorm_sc<-score(slice(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE),dtnorm)
slice_score_dtnorm_mc<-score(slice(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE),dtnorm)

slice_score_dtexp_sc<-score(slice(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE),dtexp)
slice_score_dtexp_mc<-score(slice(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE),dtexp)

slice_score_dunif_mix_sc<-score(slice(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE),dunif_mix)
slice_score_dunif_mix_mc<-score(slice(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE),dunif_mix)

slice_score_dtnorm_mix1_sc<-score(slice(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE),dtnorm_mix1)
slice_score_dtnorm_mix1_mc<-score(slice(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE),dtnorm_mix1)

slice_score_dtnorm_mix2_sc<-score(slice(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE),dtnorm_mix2)
slice_score_dtnorm_mix2_mc<-score(slice(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE),dtnorm_mix2)

score_slice_names=c("slice_score_dbetann_sc","slice_score_dbetann_mc","slice_score_dtnorm_sc","slice_score_dtnorm_mc",
                     "slice_score_dtexp_sc","slice_score_dtexp_mc","slice_score_dunif_mix_sc","slice_score_dunif_mix_mc",
                     "slice_score_dtnorm_mix1_sc","slice_score_dtnorm_mix1_mc","slice_score_dtnorm_mix2_sc","slice_score_dtnorm_mix2_mc")

score_slice=c(slice_score_dbetann_sc,slice_score_dbetann_mc,slice_score_dtnorm_sc,slice_score_dtnorm_mc,
              slice_score_dtexp_sc,slice_score_dtexp_mc,slice_score_dunif_mix_sc,slice_score_dunif_mix_mc,
              slice_score_dtnorm_mix1_sc,slice_score_dtnorm_mix1_mc,slice_score_dtnorm_mix2_sc,slice_score_dtnorm_mix2_mc)

score_slice_data=data.frame(cbind(score_slice_names, score_slice))

# save as a dataframe

save(score_slice_data, file="score_slice_data.Rdata")



###### recording time --- SINGLE CORE (slice sampler)

slice_dbetann=rbind(
  system.time(slice(n=s1, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dbetann, range=c(0,1), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dbetann, range=c(0,1), mc=FALSE))
)

slice_dtnorm=rbind(
  system.time(slice(n=s1, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dtnorm, range=c(-3,3), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dtnorm, range=c(-3,3), mc=FALSE))
)

slice_dtexp=rbind(
  system.time(slice(n=s1, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dtexp, range=c(0,6), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dtexp, range=c(0,6), mc=FALSE))
)

slice_dunif_mix=rbind(
  system.time(slice(n=s1, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dunif_mix, range=c(-3,4), mc=FALSE))
)

slice_dtnorm_mix1=rbind(
  system.time(slice(n=s1, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dtnorm_mix1, range=c(0,10), mc=FALSE))
)

slice_dtnorm_mix2=rbind(
  system.time(slice(n=s1, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(slice(n=s2, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(slice(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE)),
  system.time(slice(n=s4, dfunc=dtnorm_mix2, range=c(-4,4), mc=FALSE))
)


slice_single=c(
  "slice_dbetann",
  "slice_dtnorm",
  "slice_dtexp",
  "slice_dunif_mix",
  "slice_dunif_mix",
  "slice_dtnorm_mix1",
  "slice_dtnorm_mix2"
)

s=c(100, 
    10000,
    1000000,
    10000000)

names=c(rep(slice_single[1],4), rep(slice_single[2],4), rep(slice_single[3],4),
        rep(slice_single[4],4),rep(slice_single[5],4),rep(slice_single[6],4),
        rep(slice_single[7],4)  )


iteration=rep(s,7)

slice_all=c(slice_dbetann[,3], slice_dtnorm[,3],
             slice_dtexp[,3], slice_dunif_mix[,3], slice_dunif_mix[,3],
            slice_dtnorm_mix1[,3], slice_dtnorm_mix2[,3])

slice_data=data.frame(cbind(names, iteration, "time"=slice_all) )

## save as a dataframe

save(slice_data, file="slice_data.Rdata")



######  recording time --- MULTI CORE (slice sampler)

slice_dbetann_T=rbind(
  system.time(slice(n=s1, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(slice(n=s2, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(slice(n=s3, dfunc=dbetann, range=c(0,1), mc=TRUE)),
  system.time(slice(n=s4, dfunc=dbetann, range=c(0,1), mc=TRUE))
)

slice_dtnorm_T=rbind(
  system.time(slice(n=s1, dfunc=dtnorm, range=c(-3,3), mc=TRUE),
              system.time(slice(n=s2, dfunc=dtnorm, range=c(-3,3), mc=TRUE)),
              system.time(slice(n=s3, dfunc=dtnorm, range=c(-3,3), mc=TRUE)),
              system.time(slice(n=s4, dfunc=dtnorm, range=c(-3,3), mc=TRUE))
  )
  
  slice_dtexp_T=rbind(
    system.time(slice(n=s1, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(slice(n=s2, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(slice(n=s3, dfunc=dtexp, range=c(0,6), mc=TRUE)),
    system.time(slice(n=s4, dfunc=dtexp, range=c(0,6), mc=TRUE))
  )
  
slice_dunif_mix_T=rbind(
    system.time(slice(n=s1, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(slice(n=s2, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(slice(n=s3, dfunc=dunif_mix, range=c(-3,4), mc=TRUE)),
    system.time(slice(n=s4, dfunc=dunif_mix, range=c(-3,4), mc=TRUE))
  )
  
slice_dtnorm_mix1_T=rbind(
    system.time(slice(n=s1, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(slice(n=s2, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(slice(n=s3, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE)),
    system.time(slice(n=s4, dfunc=dtnorm_mix1, range=c(0,10), mc=TRUE))
  )
  
slice_dtnorm_mix2_T=rbind(
    system.time(slice(n=s1, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(slice(n=s2, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(slice(n=s3, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE)),
    system.time(slice(n=s4, dfunc=dtnorm_mix2, range=c(-4,4), mc=TRUE))
  )
  
  
slice_single_T=c(
    "slice_dbetann_T",
    "slice_dtnorm_T",
    "slice_dtexp_T",
    "slice_dunif_mix_T",
    "slice_dunif_mix_T",
    "slice_dtnorm_mix1_T",
    "slice_dtnorm_mix2_T"
  )
  
  s=c(100, 
      10000,
      1000000,
      10000000)
  
  names=c(rep(slice_single_T[1],4), rep(slice_single_T[2],4), rep(slice_single_T[3],4),
          rep(slice_single_T[4],4),rep(slice_single_T[5],4),rep(slice_single_T[6],4),
          rep(slice_single_T[7],4)  )
  
  
  iteration=rep(s,7)
  
  
slice_all_T=c(slice_dbetann_T[,3], slice_dtnorm_T[,3],
              slice_dtexp_T[,3], slice_dunif_mix_T[,3], slice_dunif_mix_T[,3],
              slice_dtnorm_mix1_T[,3], slice_dtnorm_mix2_T[,3])
  
slice_data_T=data.frame(cbind(names, iteration, "time"=slice_all_T) )
  
  ## save as a dataframe
  
save(slice_data_T, file="slice_data_T.Rdata")
  
