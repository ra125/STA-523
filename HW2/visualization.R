
#source("check_packages.R")
check_packages(c("rgdal","sp","rgeos","maptools"))

######## Download the US map file
dir("~/Sta523/data/us-atlas/shp/us/","*.shp")
suppressMessages(library(rgdal))
ogrInfo("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered")
states = readOGR("/home/vis/cr173/Sta523/data/us-atlas/shp/us/","states-unfiltered", stringsAsFactors=FALSE)

########### Save the location info of Denny's as Spatial Points
x=as.double(as.matrix(dennys_data$longitude))
y=as.double(as.matrix(dennys_data$latitude))
den_coord=cbind(x,y)
den = SpatialPoints(as.data.frame(den_coord)) 

########### Save the location info of La Quinta as matrix (Kungang already created this)
lq_coord = SpatialPoints(lqlatlong) #doesn't run for now
lq =SpatialPoints(as.data.frame(lq_coord))



######### Plot them on the US map.. Not so informative..
par(mar=c(2,2,1,1))
plot(states, axes=TRUE)
plot(den, axes=TRUE, add=TRUE, pch=16, cex=0.1, col="red")
plot(lq, axes=TRUE, add=TRUE, pch=16, cex=0.1, col="blue")



########## Now on sample states. We pick Califoria and North Carolina

###### California
dennys_ca = which(dennys_data$state=="CA")
den_coord_ca<-den_coord[dennys_ca,]
den_ca = SpatialPoints(data.frame(den_coord_ca)) 

laquinta_ca = which(data.lq$state=="CA")
lq_coord_ca<-lq_coord[laquinta_ca,]
lq_ca = SpatialPoints(data.frame(lq_coord_ca)) 

########## Now plot California
par(mar=c(2,2,1,1))
plot(states[states$STATE == "California",], col=c("lightgrey"), axes=TRUE)
points(den_ca, col="red", pch=16, cex=0.6)
points(lq_ca, col="blue", pch=16, cex=0.6)
par(mar=c(2,2,1,1))
legend("bottomleft",c("La Quinta","Denny's"),col=c("blue","red"),pch=16)


###### North Carolina
dennys_nc = which(dennys_data$state=="NC")
den_coord_nc<-den_coord[dennys_nc,]
den_nc = SpatialPoints(data.frame(den_coord_nc)) 

laquinta_nc = which(data.lq$state=="NC")
lq_coord_nc<-lq_coord[laquinta_nc,]
lq_nc = SpatialPoints(data.frame(lq_coord_nc)) 

########## Now plot North Carolina

plot(states[states$STATE == "North Carolina",], axes=TRUE)
points(den_nc, col="red", pch=16, cex=0.6)
points(lq_nc, col="blue", pch=16, cex=0.6)
legend("bottomleft",c("La Quinta","Denny's"),col=c("blue","red"),pch=16)


#### We could plot some other states as well..

# dennys_fl = dennys_data[dennys_data$state=="FL",]
# den_coord_fl<-cbind(dennys_fl$longitude, dennys_fl$latitude)
# den_fl = SpatialPoints(data.frame(den_coord_fl)) 
# dennys_tx = dennys_data[dennys_data$state=="TX",]
# den_coord_tx<-cbind(dennys_tx$longitude, dennys_tx$latitude)
# den_tx = SpatialPoints(den_coord_tx) 




