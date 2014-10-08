
source("check_packages.R")
check_packages(c("data.table","ggmap"))
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations.csv",sep=",")

head(park)
precinct<-park$"Violation Location"
house_street<-cbind(park$"House Number", park$"Street Name")
house_street[1:10,]
address<-apply(house_street,1,paste,collapse=" ")
address[1:10]
rm(house_street)
rm(park)

### we only work with address and precinct from now. rm all other data frame that takes up RAM space!
geocode(address, 
        output="latlong", 
        override_limit=FALSE) #geocode requires 'ggmap'



