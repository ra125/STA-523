###### Task 1  (Geocoding)

#Load necessary packages
source("check_packages.R")
check_packages(c("data.table","ggmap","rgdal","maptools", "dplyr","stringr","lubridate"))
rm(check_packages)  #Remove anything that could take up memory

#Load the Biggish NYC parking violations datafile
#test with small subset of data first
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations_small.csv",sep=",")
park<-park[park$'Violation County'=="NY",] #Filtering only NY for Manhattan
park$'Issue Date'=mdy(park$'Issue Date') 
colnames(park$'Issue Date')="Issue.Date" 
park<-filter(park, Issue.Date>"2013-07-31", Issue.Date<"2014-07-01") #Filtering the correct dates

#Extract the necessary variables
precinct<-as.numeric(park$"Violation Location")
address<-paste(park$"House Number", park$"Street Name")
street<-park$"Street Name"
cross<-park$"Intersecting Street"
street<-str_trim(street, side = "right")
cross<-str_trim(street, side = "left")
streets<-paste(street,":",cross)

#Create a new dataframe with necessary variables
d=data.frame("precinct"=as.numeric(precinct), 
             "address"=address, 
             "street"=street,
             "cross"=cross,
             "streets"=streets)

d=d[d$]

#Remove the original data frame and the temporary var names to save memory
rm(house_street)
rm(precinct)
rm(address)
rm(street)
rm(cross)
rm(park)


#Load 'pluto' data to geocode
basepath<-getwd()
setwd("/home/vis/cr173/Sta523/data/parking/pluto/Manhattan/")
pluto<-readShapeSpatial("MNMapPLUTO")
pluto_shape<-pluto

# plot(pluto, axes=TRUE)  ### If you wanna see what the data looks like..
pluto<-data.frame("address"=pluto$Address, 
                  "precinct"=pluto$PolicePrct, 
                  coordinates(pluto)) 
setwd(basepath)
rm(basepath)


#### We exlude observations that are not associated with the correct Manhattan precincts (1:34)
d<-d[which(d[,1] < 35),]
pluto<-pluto[which(pluto$precinct<35 & pluto$precinct>0),]