### R function explanation

firstly, detecting the type of function by using as.charactor(substitute()).

Once it defines the type of function,
each function follows the logic below.

for (beta), we can use (rbeta) function for sampling

for (truncated exponential), firsty, making an empty vector
and then generate and save samples by using (rnorm) function, if conditions are met

for (truncated exponential), sampling method is similar to the above (truncated normal).

for (uniform mixture), after making an empty vector, we can generate probablity from (runif) function.
and then if probabilty <= 0.6, generate sample from runif(1,-3,-1). 
if 0.6<probablity<=0.7, generate sample from runif(1,-1,1). else generate from runif(1,1,4)

for (truncated normal mixture1), after making an empty vector, in the same way above, we can generate probablity from (runif) function.
ans then if probablity <= 0.5, generate sample from rnorm(1,2,2). else generate from rnorm(1,6,1).

for (truncated normal mixture2), sampling method is similar to the above (truncated normal mixture1)



### R function 

R = function(dfunc,n,mc=FALSE){
  
  stopifnot(is.function(dfunc) & is.numeric(n))
  
  if(as.character(substitute(dfunc))=="dbetann"){
    v=rbeta(n,0.9,0.9)}
  
  if(as.character(substitute(dfunc))=="dtnorm"){
    v=c()
    for(i in 1:n){
      x=rnorm(1)
      if(x > -3 & x < 3){
        v=c(v,x)}
      else {v=v} }}
  
  if(as.character(substitute(dfunc))=="dtexp"){
    v=c()
    for(i in 1:n) {
      x=rexp(1,rate=1/3)
      if(x > 0 & x < 6){ 
        v=c(v,x)}
      else {v=v} }}
  
  if(as.character(substitute(dfunc))=="dunif_mix"){
    v=c()
    for(i in 1:n){
      prob <- runif(1, 0, 1)
      if(prob <= 0.6){
        x=runif(1,-3,-1)
        v[i]=x}
      else if(prob > 0.6 & prob <= 0.7){ 
        x=runif(1,-1,1)
        v[i]=x}
      else{ 
        x=runif(1,1,4)
        v[i]=x} }}
  
  if(as.character(substitute(dfunc))=="dtnorm_mix1"){
    v=c()
    for(i in 1:n){
      prob <- runif(1, 0, 1)
      if(prob <= 0.5){
        x=rnorm(1,2,2)
        if(x > 0 & x < 10){
          v=c(v,x)}
        else { v=v }} 
      else{ 
        x=rnorm(1,6,1)
        if(x > 0 & x < 10){
          v=c(v,x)}
        else { v=v } }}}
  
  if(as.character(substitute(dfunc))=="dtnorm_mix2"){
    v=c()
    for(i in 1:n){
      prob <- runif(1, 0, 1)
      if(prob <= 0.45){
        x=rnorm(1,-4,1)
        if(x > -4 & x < 4){
          v=c(v,x)}
        else { v=v }} 
      else if(prob > 0.45 & prob <= 0.9){ 
        x=rnorm(1,4,1)
        if(x > -4 & x < 4){
          v=c(v,x)}
        else { v=v }} 
      else{ 
        x=rnorm(1,0,0.5)
        if(x > -4 & x < 4){
          v=c(v,x)}
        else { v=v }} }}
  
  return(v)
}




### Testing

dbetann = function(x){dbeta(x,0.9,0.9)}
dtnorm = function(x){ifelse(x < -3 | x > 3, 0, dnorm(x)/0.9973002)}
dtexp = function(x){ifelse(x < 0 | x > 6, 0, dexp(x, rate=1/3)/0.8646647)}
dunif_mix = function(x){
  ifelse(x >= -3 & x < -1, 0.6*dunif(x,-3,-1),
         ifelse(x >= -1 & x <  1, 0.1*dunif(x,-1, 1),
                ifelse(x >=  1 & x <  4, 0.3*dunif(x, 1, 4), 
                       0)))}
dtnorm_mix1 = function(x){
  ifelse(x < 0 | x > 10, 
         0, 
         ( 0.5*dnorm(x,mean=2,sd=2)
           +0.5*dnorm(x,mean=6,sd=1))/0.90059152)}
dtnorm_mix2 = function(x){
  ifelse(x < -4 | x > 4, 
         0, 
         ( 0.45*dnorm(x,mean=-4)
           +0.45*dnorm(x,mean= 4)
           +0.1 *dnorm(x,mean= 0,sd=0.5))/0.4999683)
}



