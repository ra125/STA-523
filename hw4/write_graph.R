#Task for function write_graph
#1. Write the vertex;
#2. Write seperate ->;
#3. Write weight;
#4. Write isolate nodes;
#5. If a node doesn't have a name, asign the integer index of its in the list as its name

write_graph=function(graphobj = g,filename = gname)#g is the object of graph; gname is the name of file
{
  source("is_valid.R")
  if (is.list(g)==F) 
  {
    print("The input is not a list!")
    return(NULL)
  }
  if (is_valid(g)==F)
  {
    print("The input is not a valid graph")
    return(NULL)
  }
  #number of nodes
  nnode=length(g)
  #names of nodes, if there is no name, it will return ''
  vname<-names(g)
  #asign the index of nodes as their name, if no name
  vname[vname[]=='']=(1:length(vname))[vname[]=='']
  #extract the 'to' node and corresponding weight
  #only when use 'rbind' can later 'unlist' give alternative edge and weight
  tonode_weight<-lapply(g,function(x)
    {
      #the lenght of edges and weights must be the same to be valid for a graph
      edges <- rbind(x$edges,x$weights)
      return(edges)
    })
  #transfer list to two-columned data.frame
  #note that the node without 'to' node wouldn't have a row in data frame below
  tonode_weight<-matrix(unlist(tonode_weight),ncol = 2,byrow = T)
  #how many 'to' nodes for each node and transfer to a vector
  ntonode<-unlist(lapply(g,function(x) {return(length(x$edges))}))
  #find those node with zero 'to' node
  zerotonode.index<-as.integer(ntonode[]==0)
  #generate 'from' nodes for each edge
  #if no 'to' node, don't generate for now
  fromnode<-rep(vname,ntonode)
  nline<-length(fromnode)
  #graph data ready to save into file. it is a data.frame
  #those nodes with at least one 'to' node
  gdata1<-cbind(fromnode, rep(' -> ', nline), 
               vname[tonode_weight[,1]], 
               rep(' [weight=',nline),
               tonode_weight[,2],
               rep(']',nline),
               rep(';',nline))
  #those nodes without 'to' node
  nline0<-sum(zerotonode.index)
  gdata0<-cbind(rep(vname,zerotonode.index), 
                matrix('',nrow = nline0,ncol = 5),
                rep(';',nline0))
  gdata<-rbind(gdata1,gdata0)
  write.table(gdata, file=paste0(gname,'.txt'), 
              append=F, 
              quote=F, 
              sep="", 
              row.names=F,
              col.names=F)
}

write_graph(g3, "g3")
