## load required packages
source("check_packages.R")
check_packages(c("stringr"))

names=c("UT","DC","KS","AL","HI")   

for(i in names){
  
  # name=names 
  
  fileName <- paste0("dennys/",i,".html")
  s <- readChar(fileName, file.info(fileName)$size)
  
  # Extracting relevant data
  
  each=as.character(str_match_all(s,"<poi>(.*?)</poi>"))
  name=as.data.frame(str_match_all(each,"<name>(.*?)</name>"))
  name=as.data.frame(name[1:1000,2])
  uid=as.data.frame(str_match_all(each,"<uid>([0-9-]*)</uid>"))
  uid=unlist(uid[1:1000,2])
  address1=as.data.frame(str_match_all(each,"<address1>(.*?)</address1>"))
  address1=unlist(address1[1:1000,2])
  # address2=as.data.frame(str_match_all(each,"<address2>(.*?)</address2>"))
  # address2=as.data.frame(address2[1:1000,2]) #---- nobody has address2 in US
  city=as.data.frame(str_match_all(each,"<city>(.*?)</city>"))
  city=unlist(city[1:1000,2]) 
  state=as.data.frame(str_match_all(each,"<state>(.*?)</state>"))
  state=unlist(state[1:1000,2])
  country=as.data.frame(str_match_all(each,"<country>(.*?)</country>"))
  country=unlist(country[1:1000,2])
  zip=as.data.frame(str_match_all(each,"<postalcode>(.*?)</postalcode>"))
  zip=unlist(zip[1:1000,2])
  phone=as.data.frame(str_match_all(each,"<phone>(.*?)</phone>"))
  phone=unlist(phone[1:1000,2])
  # fax=as.data.frame(str_match_all(each,"<fax>(.*?)</fax>"))
  # fax=as.data.frame(fax[1:1000,2])   #----- nobody has fax..
  latitude=as.data.frame(str_match_all(each, "<latitude>([0-9.0-9]*)</latitude>"))
  latitude=unlist(latitude[1:1000,2])
  longitude=as.data.frame(str_match_all(each,"<longitude>(-[0-9.0-9]*)</longitude>"))
  longitude=unlist(longitude[1:1000,2])
  
  data=cbind(name, uid, address1, city, state, country, zip, phone, "latitude"=latitude, "longitude"=longitude)
  assign(i, data)
  
}

################### add them together and make the final data

dennys_data=rbind(UT, DC, AL, HI, KS)

dennys_data=dennys_data[!duplicated(dennys_data$uid),]
dim(dennys_data)
#removing Dennys outside US
dennys_data=dennys_data[dennys_data$country=="US",]
# dennys_data
dim(dennys_data)


# Save results as Rdata file
save(dennys_data, file="dennys/dennys_data.Rdata")

