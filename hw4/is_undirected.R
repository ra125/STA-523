is_undirected = function(g) {
  
  source("is_valid.R")
  
  if(is_valid(g)==FALSE)
  {
    stop("Graph is not valid")
  }
  
  n = length(g)
  matrix = matrix(0, nrow=n, ncol=n)
  index=data.frame(Edges=1:n)
  
  for(i in 1:n){
    if(length(g[[i]]$edges)!=0){
      valid=data.frame(Edges=g[[i]]$edges,Weights=g[[i]]$weights)
      output=merge(index,valid,all.x=TRUE)
      matrix[i,]=output$Weights
    }else{
        matrix[i,]=NA
      }
  }
   
# Check if all directed edges have a complemetary directed edge with the same weight in the opposite direction.
    
  matrix[is.na(matrix)]=0.00
  if(identical(matrix,t(matrix))){
      return(TRUE)
    }else{
      return(FALSE)
      }
  }