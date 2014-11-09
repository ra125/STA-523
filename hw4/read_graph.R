library(testthat)
library(methods)

#1. Read a DOT language of graph data from a txt file
#2. Once the format is not ritht, report error
#3. If not weight for every line, asigned default weight to every edge
#4. Return an object of the graph

read_graph=function(filename)
{
  gdata<-read.table(filename, sep="\n", quote="")
  gdata<-unlist(gdata)
  #given a vector of characters, return NA if all NA; return the str
  strv_na<-function(strv)
  {
    n=length(strv)
    for (i in 1:n)
    {
      if (!is.na(strv[i])) return(strv[i])
    }
    return(NA)
  }
  
  #
  fromnode=rep(NA,length(gdata))
  tonode=rep(NA,length(gdata))
  weight=rep(NA,length(gdata))
  for (i in 1:length(gdata))
  {
    #style of edge
    #style of from node
    styfnode1<-str_match(gdata[i],"^([[:alnum:]]+) -> ")[2]
    styfnode2<-str_match(gdata[i],"^\"([[:alnum:] ]+)\" -> ")[2]
    #style of to node without weight   
    stytnode1<-str_match(gdata[i]," -> ([[:alnum:]]+) [[]+weight=[[:digit:]e+]+[]]+;$")[2]
    stytnode2<-str_match(gdata[i]," -> \"([[:alnum:] ]+)\" [[]weight=[[:digit:]e+]+[]];$")[2]
    #style of to node with weight
    stytnode3<-str_match(gdata[i]," -> ([[:alnum:]]+);$")[2]
    stytnode4<-str_match(gdata[i]," -> \"([[:alnum:] ]+)\";$")[2]
    
    #style of single node
    stysnode1<-str_match(gdata[i],"^([[:alnum:]]+);$")[2]
    stysnode2<-str_match(gdata[i],"^\"([[:alnum:] ]+)\";$")[2]
    
    fnode<-strv_na(c(styfnode1,styfnode2))
    tnode<-strv_na(c(stytnode1,stytnode2,stytnode3,stytnode4))
    snode<-strv_na(c(stysnode1,stysnode2))
    if (!is.na(fnode) & !is.na(tnode))
    {
      fromnode[i]=fnode
      tonode[i]=tnode
      if (is.na(str_match(gdata[i]," [[]weight=([[:digit:]e+]+)[]];$")[2])) 
      {weight[i]=1}
      else
      {weight[i]=str_match(gdata[i]," [[]weight=([[:digit:]e+]+)[]];$")[2]}
    }
    else
    {
      if (!is.na(snode)) 
      {
        fromnode[i]=snode
#         tonode[i]=integer(0)
#         weight[i]=numeric(0)
      }
      else
      {
        stop("The graph file is not valid!")
      }
    }
  }
  allnode<-sort(unique(c(fromnode,tonode)))
  nnode<-length(allnode)
  gobj<-rep(list(list("edges"=integer(0),"weights"=numeric(0))),nnode)
  for (i in 1:nnode)
  {
    names(gobj)[i]=allnode[i]
  }
  for (i in 1:length(gdata))
  {
    if (!is.na(tonode[i]))
    {
      gobj[[fromnode[i]]]$edges=c(gobj[[fromnode[i]]]$edges,(1:nnode)[allnode[]==tonode[i]])
      gobj[[fromnode[i]]]$weights=c(gobj[[fromnode[i]]]$weights,as.numeric(weight[i]))
    }
  }
  return(gobj)
}