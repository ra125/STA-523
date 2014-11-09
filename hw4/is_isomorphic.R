#1. Check names of vertices and fill names
#2. Order and unique the vertices based on names
#3. If order and unique pass, check the edges and weights

is_isomorphic=function(g1, g2)
{
    source("is_valid.R")
    
    if (is_valid(g1)==F & is_valid(g2)==F)
    {
      stop("Both graphes are not valid")
    } else 
    {
      if (is_valid(g1)==F)
      {
        stop("Graph 1 is not valid")
      } else
      {
        if (is_valid(g2)==F)
        {
          stop("Graph 2 is not valid")
        }
      }
    }
  #number of nodes
  nnode1=length(g1)
  nnode2=length(g2)
  #if number of nodes doesn't match, the two graphes are not the same
  if (nnode1 != nnode2)
  {
    return(FALSE)
  }
  #The complete problem of determining whether two graphs are the same should be following
  #Complete Problem: No matter what are the indices or name of vertices (any permutation) for two graphs,
  #there exist one permutation for each of the graph so that these two graphs have the same
  #indices number, weights, and structure.
  #The simplified problme in homework: when comparing the children node, we can use label name
  
  
  #names of nodes, if there is no name, it will return ''
  vname1<-names(g1)
  vname2<-names(g2)
  #asign the index of nodes as their name, if no name
  vname1[vname1[]=='']=(1:length(vname1))[vname1[]=='']
  vname2[vname2[]=='']=(1:length(vname2))[vname2[]=='']
  #unique and sort their name and compare them
  if (identical(sort(unique(vname1),decreasing = FALSE), 
                sort(unique(vname2),decreasing = FALSE))==FALSE)
  {
    return(FALSE)
  }
  for (i in 1:length(vname1))
  {
    #Find out which nodes have null outgoing nodes
    #Note here should use 'g1[[vname1[i]]]$edges' not 'g1$vname1[i]$edges'
    #don't use is.null, becasue it wouldn't pass #63 line test
    if (length(g1[[vname1[i]]]$edges) != length(g2[[vname1[i]]]$edges))
    {
      return(FALSE)
    } else
    {
      if (length(g1[[vname1[i]]]$edges)==0 & length(g2[[vname1[i]]]$edges)==0)
      {
        next
      }
    }
    #Use name of vertices to compare
    ii1<-order(vname1[g1[[vname1[i]]]$edges],g1[[vname1[i]]]$weights)
    ii2<-order(vname2[g2[[vname1[i]]]$edges],g2[[vname1[i]]]$weights)
    #If the edges and weights are not exactly match
    if (identical(rbind(vname1[g1[[vname1[i]]]$edges],g1[[vname1[i]]]$weights)[,ii1],
                  rbind(vname2[g2[[vname1[i]]]$edges],g2[[vname1[i]]]$weights)[,ii2])==FALSE)
    {
      return(FALSE)
    }
  }
  return(TRUE)
}
