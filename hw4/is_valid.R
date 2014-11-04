library(testthat)
library(methods)

#fuction 2: is_valid (Sophie)
## Validate the graph object to ensure that it meets all requirements - 
#Check that object is a list of lists. Check if there are names for the primary list that they are all unique. 
#Check that each secondary list contains only edges and weights vectors that are of the appropriate type. 
#Check that there are not any edges to non-existent vertices. 
#Check that all weights are not less than or equal to 0. 
#Check that every edge has a weight.#If no weight, is it still valid?

is_valid=function(input){
  
  # Testing if the input has a list object
  if(typeof(input[[1]])=="list"){
    t1<-TRUE
  }
  else {
    t1<-FALSE
  }
  
  # Testing if the names of the lists in object are all unique
  names=names(input)
  d_names=unique(names)
  if(length(names)==length(d_names)){
    t2<-TRUE
  }
  else{
    t2<-FALSE
  }
  
  # Testing if the object has both weights and edges as lists
  t3_obj1<-grep("weights",names)
  t3_obj2<-grep("edges",names)
  if(sum(t3_obj1)>0 & sum(t3_obj2)>0){
    t3<-TRUE
  }
  else{
    t3<-FALSE
  }
  
  # Testing if the contents in the lists have the mode of numeric
  if(is.numeric(input[[1]]$edges)==TRUE & is.numeric(input[[1]]$weights)==TRUE){
    t4<-TRUE
  }
  else{
    t4<-FALSE
  }
  
  # Testing that there are not any edges to non-existent vertices. 
  n=length(input)
  temp=NULL
  t_obj1=list()
  for(i in 1:n){
    t_obj1[[i]]<-input[[i]]$edges
    temp=c(temp,t_obj1[[i]])
  }
  t_obj2=(1:n)
  
  if(order(unique(temp))==order(unique(t_obj2))) {
    t5<-TRUE
  }
  else{
    t5<-FALSE
  }
  # Testing if the weights are greater than 0.
  if(input[[1]]$weights>0){
    t6<-TRUE
  }
  else{
    t6<-FALSE
  }
  
  
  # Testing if the object passes all tests above
  each<-cbind(t1,t2,t3,t4,t5,t6)
  if(sum(which(each==FALSE))>0) {
    return(FALSE)
  }
  else{
    return(TRUE)
  }
  
}