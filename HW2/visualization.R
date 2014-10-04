

source("check_packages.R")
check_packages("rgdal")

dir("~/Sta523/data/us-atlas/shp/us/","*.shp")

suppressMessages(library(rgdal))
ogrInfo("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered")
states = readOGR("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered", stringsAsFactors=FALSE)

library(sp)  

# sp = SpatialPoints(data.frame(x=c(10,40,20,30),y=c(40,30,20,10)))
# 
# par(mar=c(4,4,1,1))
# plot(sp, axes=TRUE)
# points(coordinates(sp), pch=16)
dennys_data=data.dennys

den_coord<-cbind(-1*(dennys_data$longitude), dennys_data$latitude)
den = SpatialPoints(den_coord) 
lq_coord<-cbind(lq_data$longitude, lq_data$latitude)
lq = SpatialPoints(lq_coord)

# plot(states, col="lightgrey", axes=TRUE)




plot(states[states$ORDER_ADM %in% 1:48,], col="lightgrey", axes=TRUE)
points(den, col="red")
points(lq, col="blue")




########## sample states
plot(states[states$STATE == "California",], col=c("lightgrey","lightblue"), axes=TRUE)

denn_cal=denn[STATE==CA]
lqt_cal=lqt[STATE=CA]
points(denn_cal, col="red")
points(lqt_cal, col="blue")
