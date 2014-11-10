source("is_valid.R")
source("is_undirected.R")

min_span_tree=function(g)
{
  if(is_valid(g)==FALSE)
  {
    stop("Graph is not valid")
  }
  
  if(is_undirected(g)==FALSE)
  {
    stop("Graph is directed")
  }
  
  #Adjacency matrix
  n=length(g)
  names=names(g)
  m=matrix(rep(0,n^2),nrow=n,ncol=n)
  
  for(i in 1:n)
  {
    count=1
    for(j in g[[i]]$edges)
    {
      m[i,j]=g[[i]]$weights[count]
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
  
  # Prims Algorithm
  V=n
  
  #Function to find the minimum key value
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
  
  #Main Prims loop explained in writeup
  
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
  
  for(p in 1:length(parent))
  {
    if(parent[p]==-1)
    {
      m[p,]=0
    }
    m[p,-parent[p]]=0
  }
  
  for(i in 1:nrow(m))
  {
    for(j in 1:ncol(m))
    {
      m[i,j]=max(m[j,i],m[i,j])
    }
  }
  
  #Converting to list
  
  al = list()
  for(i in 1:nrow(m)) {
    if(is.null(names)==FALSE)
      {
      v = names[i]
      } else {v=i}
      al[[v]] = list(edges=c(), weights=c())
      for(j in 1:ncol(m)) {
        if(m[i,j] != 0) {
          al[[v]]$edges = c(al[[v]]$edges, j)
          al[[v]]$weights = c(al[[v]]$weights, m[i,j])
        }
      }
    }
  
  return(al)
}