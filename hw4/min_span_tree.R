library(testthat)
library(methods)

min_span_tree=function(g)
{
  source(is_valid.R)
  source(is_undirected.R)
  source(is_connected.R)
  
  if(is_valid(g)==FALSE)
  {
    stop("Graph is not valid")
  }
  
  if(is_undirected(g)==FALSE)
  {
    stop("Graph is directed")
  }
  
  if(is_connected(g)==FALSE)
  {
    stop("Graph is not connected")
  }
  
  n=length(g1)
  m=matrix(rep(0,n^2),nrow=n,ncol=n)
  
  for(i in 1:n)
  {
    count=1
    for(j in g1[[i]]$edges)
    {
      m[i,j]=g1[[i]]$weights[count]
      count=count+1
    }
  }
  
  for(i in 1:nrow(m))
  {
    for(j in 1:ncol(m))
    {
      if(i==j)
      {
        m[i,j]=0
      }
    }
  }
  
  V=n
  
  minKey=function(key, mstSet)
  {
    min=Inf
    for(v in 1:V)
    {
      if(mstSet[v]==FALSE && key[v]<min)
      {
        min=key[v]
        min_index=v
      }    
    }
    return(min_index)
  }
  
  key=rep(Inf,V)
  parent=rep(Inf,V)
  mstSet=rep(0,V)
  
  key[1]=0
  parent[1]=-1
  
  for(count in 1:V)
  {
    u=minKey(key,mstSet)
    mstSet[u]=TRUE
    
    for(v in 1:V)
    {
      if(m[u,v] && mstSet[v]==FALSE && m[u,v]<key[v])
      {
        parent[v]=u
        key[v]=m[u,v]
      }
    }
  }
  
  #create return graph using parent and m
  
}