###### Task 1  (Geocoding)


source("check_packages.R")
check_packages(c("data.table","ggmap","rgdal","maptools"))
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations.csv",sep=",")

head(park)
precinct<-park$"Violation Location"
house_street<-cbind(park$"House Number", park$"Street Name")
address<-apply(house_street,1,paste,collapse=" ")
rm(house_street)
rm(park)

### we only work with address and precinct from now. rm all other data frame that takes up RAM space!
###geocode has 2500 requests limit a day


howmany <-1:100
# howmany<-sample(1:length(address),2500,replace=FALSE)
# howmany<-c(1:length(address))

latlon<-geocode(address[howmany], 
        output="latlon", 
        override_limit=FALSE) #geocode requires 'ggmap'

d<-cbind(latlon,precinct)
d[1:10,]

rm(address)
rm(precinct)
rm(howmany)


######## Task 2 (Subsetting -- We will use 95% of the datapoints)
q_lon<-matrix(quantile(d$lon, probs=c(0.025, 0.975), na.rm=TRUE))
q_lat<-matrix(quantile(d$lat, probs=c(0.025, 0.975), na.rm=TRUE))
pres<-(which( q_lon[1,1]< d$lon & d$lon<q_lon[2,1] & q_lat[1,1]< d$lat & d$lat<q_lat[2,1]))
data<-d[pres,]

######## Now, the subsetted data is called "d"  Remove all else
rm(d)
rm(q_lon)
rm(q_lat)
rm(pres)



##### Task 3
##### Plot the latitudes and longitudes in Manhattan
#### the following code won't run for now because Colin's nybb folder has a permission setting.
#### I've asked him to change that.


basepath<-getwd()
setwd("/home/vis/cr173/Sta523/data/parking/nybb")
nyc<-readShapeSpatial("nybb")
setwd(basepath)
rm(basepath)
manhattan<-nyc[nyc$BoroName=="Manhattan",]
par(mar=c(2,2,1,1))
plot(nyc, axes=TRUE)
plot(manhattan, axes=TRUE)


###### Save the objects as multipoints
coord = SpatialPoints(data.frame(latlon)) 



######## Task 4: Recreate the boundary!




