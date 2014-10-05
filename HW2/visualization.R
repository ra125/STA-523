#source("check_packages.R")
check_packages(c("rgdal","sp"))

#dir("~/Sta523/data/us-atlas/shp/us/","*.shp")
suppressMessages(library(rgdal))
ogrInfo("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered")
states = readOGR("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered", stringsAsFactors=FALSE)

# sp = SpatialPoints(data.frame(x=c(10,40,20,30),y=c(40,30,20,10)))
# 
# par(mar=c(4,4,1,1))
# plot(sp, axes=TRUE)
# points(coordinates(sp), pch=16)
# dennys_data=data.dennys

den_coord=cbind(as.double(attr(dennys_data$latitude,"levels")),(as.double(attr(dennys_data$longitude,"levels"))))
den = SpatialPoints(den_coord) 


lq_coord<-cbind(lq_data$longitude, lq_data$latitude)
lq = SpatialPoints(lq_coord)

# plot(states, col="lightgrey", axes=TRUE)



plot(states[states$ORDER_ADM %in% 1:48,], axes=TRUE)
points(coordinates(den), col="red", pch=10)
points(coordinates(lq), col="blue")




########## sample states
dennys_ca = which(dennys_data$state=="CA")
den_coord_ca<-den_coord[dennys_ca,]
den_ca = SpatialPoints(data.frame(den_coord_ca)) 

dennys_nc = which(dennys_data$state=="NC")
den_coord_nc<-den_coord[dennys_nc,]
den_nc = SpatialPoints(data.frame(den_coord_nc)) 
#####

# dennys_fl = dennys_data[dennys_data$state=="FL",]
# den_coord_fl<-cbind(dennys_fl$longitude, dennys_fl$latitude)
# den_fl = SpatialPoints(data.frame(den_coord_fl)) 
# dennys_tx = dennys_data[dennys_data$state=="TX",]
# den_coord_tx<-cbind(dennys_tx$longitude, dennys_tx$latitude)
# den_tx = SpatialPoints(den_coord_tx) 

par(mar=c(4,4,1,1))
plot(states[states$STATE == "California",], col=c("lightgrey"), axes=TRUE)
points(coordinates(den_ca), col="red", pch=16)

par(mar=c(4,4,1,1))
plot(states[states$STATE == "North Carolina",], col=c("lightgrey"), axes=TRUE)
points(coordinates(den_nc), col="red", pch=16)
