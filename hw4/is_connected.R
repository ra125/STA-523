is_connected = function(g,v1,v2)
{
  source("is_valid.R")
  stopifnot(is_valid(g))
  
  if(is.na(v1) || is.na(v2))
  {
    stop("Bad labels")
  }
  
  type=c("integer","double","character")
  
  if(!typeof(v1) %in% type)
  {
    stop("Bad labels")
  }
  
  if(!typeof(v2) %in% type)
  {
    stop("Bad labels")
  }
  
  names=names(g)
  
  if(is.null(names)==FALSE)
  {
    if(typeof(v1)=="character")
    {
      v1=which(names(g)==v1)
      if(length(v1)==0)
      {
        stop("Bad labels")
      }
    }
    if(typeof(v2)=="character")
    {
      v2=which(names(g)==v2)
      if(length(v2)==0)
      {
        stop("Bad labels")
      }
    }
  }
  
  if(v1>length(g))
  {
    stop("Bad labels")
  }
  
  if(v2>length(g))
  {
    stop("Bad labels")
  }
  
  if(v1==v2)
  {
    if(v2 %in% g[[v1]]$edges)
    {
      return(TRUE)
    } else {return(FALSE)}
  }
  
  connected = function(g, v, visited = integer())
  {
    visited = c(visited, v)
    
    if (v==v2)
      return(TRUE)
    
    for(e in setdiff(g[[v]]$edges,visited))
    {
      if (connected(g, e, visited))
        return(TRUE)
    }
    
    return(FALSE)
  }
  
  if (connected(g, v1))
      return(TRUE)
  
  return(FALSE)
}