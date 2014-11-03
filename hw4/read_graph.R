#1. Read a DOT language of graph data from a txt file
#2. Once the format is not ritht, report error
#3. If not weight for every line, asigned default weight to every edge
#4. Return an object of the graph

read_graph=function(file=filename)
{
  source("is_valid.R")
  gdata<-read.table(filename,sep=";")
  fromnode<-str_match(string = gdata,pattern = "([[:alnum:]]+) -> ")
  tonode<-str_match(string = gdata,pattern = " -> ([[:alnum:]]+)")
  allnode<-order(unique(c(fromnode,tonode)))
  nnode<-length(allnode)
  gobj<-list()
  for (i in 1:nnode)
  {
    gobj[i]#how to change name of an element of a list?
  }
}