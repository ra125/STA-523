#test the function
source("is_isomorphic.R")
basepath<-getwd()
newpath<-paste0(basepath,"/tests")
setwd(newpath)
test_file("test_is_isomorphic.R")
#test_that(is_isomorphic)
setwd(basepath)
#test the function end

#test the function
source("is_isomorphic.R")
source("read_graph.R")
basepath<-getwd()
newpath<-paste0(basepath,"/tests")
setwd(newpath)
test_file("test_read_graph.R")
#test_that(is_isomorphic)
setwd(basepath)
#test the function end

#test the function
source("is_isomorphic.R")
source("read_graph.R")
source("write_graph.R")
basepath<-getwd()
newpath<-paste0(basepath,"/tests")
setwd(newpath)
test_file("test_write_graph.R")
#test_that(write_graph)
setwd(basepath)
#test the function end