###### Task 1  (Geocoding)
#setwd("~/Team6/HW3")

#Load necessary packages
source("check_packages.R")
check_packages(c("data.table","rgeos","ggmap","rgdal","maptools", "dplyr","stringr","lubridate"))

#Load the Biggish NYC parking violations datafile
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations.csv",sep=",")

#Rename required columns
setnames(park, "Violation Precinct", "Violation.Precinct")
setnames(park, "House Number", "House.Number")
setnames(park, "Street Name", "Street.Name")


#Extract the necessary variables
addr = filter(park, Violation.Precinct <= 34) %>%
  mutate(House.Number = str_trim(House.Number), Street.Name = str_trim(Street.Name)) %>%
  filter(House.Number != "" & Street.Name != "") %>%
  filter(str_detect(House.Number,"[0-9]+")) %>%
  transmute(Violation.Precinct = Violation.Precinct, addr = paste(House.Number, Street.Name)) %>%
  mutate(addr = tolower(addr))

#rm(park)


#Load 'pluto' data to geocode
basepath<-getwd()
setwd("/home/vis/cr173/Sta523/data/parking/pluto/Manhattan/")
pluto<-readShapeSpatial("MNMapPLUTO")
setwd(basepath)
rm(basepath)

tax = cbind(data.frame(coordinates(pluto)), tolower(as.character(pluto$Address)))

names(tax)[3] = "addr"


#from full name to simplified version, becasue this make str_replace_all much easier
namemap<-rbind(c("east","west","street","avenue","av","avee","road","drive","terrace"," terr","bway","1st","2nd","3rd","th "), c("e","w","st","ave","ave","ave","rd","dr","ter"," ter","brdway","1","2","3"," "))
lnm<-ncol(namemap)

for (i in 1:lnm)
{
  tax$addr<-str_replace_all(tax$addr, namemap[1,i], namemap[2,i])
  addr$addr<-str_replace_all(addr$addr, namemap[1,i], namemap[2,i])
}

#Around 1.5 mil matches after this clean up

#Better approach to clean up, but doesnt increase data points by a significant number. 
#We can do it but its takes a lot of time and space, so we have commented it for now.

#some house number has 0 at the beginning, the total number is about 1100

#hnvset<-str_detect(park$House.Number, "^0+([[:alnum:]]+)")
#hnpset<-str_detect(tax$addr, "^0+([[:alnum:]]+)")

#addrset<-str_detect(addr$addr, "^0+([[:alnum:]]+)")
#addrmat<-str_match(addr$addr[addrset], "^0+([[:alnum:]]+)")
#nset<-length(addrset)
#seqn<-c(1:nset)
#seqnmark<-seqn[addrset]#record the seq number of element detecting the pattern with first 0
#nseqnmark<-length(seqnmark)#nseqmark is 1109

#for(i in 1:nseqnmark)
#{
#  addr$addr[seqnmark[i]]<-str_replace(addr$addr[seqnmark[i]], addrmat[i,1], addrmat[i,2])
#}
#addrset1<-str_detect(addr$addr, "^0+([[:alnum:]]+)")

#However this part only increase ~100 data points in the final z

#after cleaning 1st, 2nd, 3rd, and ~th, the matching goes up to 1569809
#after delete first 0 in house number, the matching goes up to 1569912

#unique addr$addr
#univaddr<-unique(addr$addr)
#length(addr$addr)
#length(univaddr)
#unique z$addr
#unizaddr<-unique(z$addr)
#length(unizaddr)

#not.match<-anti_join(addr$addr,z$addr)
#don't work: Error in UseMethod("anti_join") : 
#no applicable method for 'anti_join' applied to an object of class "character"

#not.mav<-anti_join(addr,tax)
#not.map<-anti_join(tax,addr) #The addr only appear in tax
#taxdt<-as.data.table(tax) #need to convert tax into data.table, otherwise would report error
#not.mav<-anti_join(addr,taxdt) #The addr only appear in addr

#uni.not.mav<-unique(not.mav)
#uni.not.map<-unique(not.map)

#By inspecting data in not.map and not.mav, using format below, we can find how many data can be changed with each modification
#If the number to increase the valid data is small, then it is not worthy to loop over all of data once
#test<-str_detect(addr$addr, " terr")
#test[is.na(test)]<-F
#sum(test) #249

#test<-str_detect(addr$addr, " square$")
#test[is.na(test)]<-F
#sum(test) #821

#test<-str_detect(tax$addr, " square")
#test[is.na(test)]<-F
#sum(test) #92

#test<-str_detect(addr$addr, " sq$")
#test[is.na(test)]<-F
#sum(test) #4940

#test<-str_detect(tax$addr, " sq")
#test[is.na(test)]<-F
#sum(test) #93

# Join the dataframe
z = inner_join(tax,addr)

#plot(z$X1,z$X2,col=z$Violation.Precinct)

rm(park)
rm(pluto)


latlon=data.frame(cbind(z$X1, z$X2))
names(latlon$X1)="lon"
names(latlon$X2)="lat"
coord=SpatialPoints(latlon)
plot(coord, col=z$Violation.Precinct, pch=18, cex=0.5, axes=TRUE)
dim(latlon)


object=SpatialPointsDataFrame(coords=latlon,data=data.frame(z$Violation.Precinct))

# obj is a list of all convex hulls. ch[[1]] has convex hull for precinct number 0, ch[[2]] for precinct 1 till ch[[35]] for precinct 34.

obj = list()
ch = list()
chl = list()
true_p=c(1,5,6,7,9,10,13,14,17,18,19,20,22,23,24,25,26,28,30,32,33,34)
j = 1

for(precinct in true_p)
{
  obj[j]=SpatialPointsDataFrame(coords=coordinates(object[object$z.Violation.Precinct==precinct,]),data=data.frame(object[object$z.Violation.Precinct==precinct,]$z.Violation.Precinct))
  ch[j] = gConvexHull(obj[[j]])
  chl[j] = SpatialPolygonsDataFrame(ch[[j]],data=data.frame(precinct)) 
  j = j+1
}

# merge
merged = chl[[1]]

for(precinct in 2:length(true_p)) {
  #print(i)
  slot(slot(chl[[precinct]],"polygons")[[1]],"ID") = as.character(precinct)
  merged = rbind(merged,chl[[precinct]])
}



writeOGR(merged, "./out", "", driver="GeoJSON") # Creates out file, current version of 
# GDAL does not allow . in file names 
# so we have to rename the file afterwards
file.rename("./out", "./precinct.json")
