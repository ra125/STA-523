shortest_path(g,v1,v2)
{
  source(is_valid.R)
  if(is_valid(g)==FALSE)
  {
    stop("Graph is not valid")
  }
  
  #Ask if connected or not
  
  n=length(g)
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
  
  V=n
  
  minDistance=function(dist, sptSet)
  {
    min=Inf
    for(v in 1:V)
    {
      if(sptSet[v]==FALSE && dist[v]<=min)
      {
        min=dist[v]
        min_index=v
      }    
    }
    return(min_index)
  }
  
  dist=rep(Inf,V)
  prev=rep(Inf,V)
  sptSet=rep(0,V)
  
  dist[v1]=0
  prev[v1]=-1
  
  for(count in 1:V)
  {
    u=minDistance(dist,sptSet)
    sptSet[u]=TRUE
    
    for(v in 1:V)
    {
      if(!sptSet[v] && m[u,v] && dist[u]!=Inf && dist[u]+m[u,v]<dist[v])
      {
        dist[v]=dist[u]+m[u,v]
        prev[v]=u
      }
    }  
  }
  
  i=v2
  a=v2
  while(prev[i]!=-1)
  {
    a=cbind(a,prev[i])
    i=prev[i]
  }

  vector=rev(a) 
  
  return(vector)
  
  #add label names
  #ask if stop will give error in the main test too?
}