

names=c("UT","DC","KS","AL","HI")

for(i in names){

# name=names 

fileName <- paste0("dennys/",i,".html")
s <- readChar(fileName, file.info(fileName)$size)

# Extracting relevant data

each=as.character(str_match_all(s,"<name>(.*?)</uid>"))
name=as.data.frame(str_match_all(each,"name>(.*?)</name>"))
name=as.data.frame(name[1:1000,2])
uid=as.data.frame(str_match_all(each,"<uid>(.*?)</uid>"))
uid=as.data.frame(uid[1:1000,2])
address1=as.data.frame(str_match_all(each,"<address1>(.*?)</address1>"))
address1=as.data.frame(address1[1:1000,2])
address2=as.data.frame(str_match_all(each,"<address2>(.*?)</address2>"))
address2=as.data.frame(address2[1:1000,2])
city=as.data.frame(str_match_all(each,"<city>(.*?)</city>"))
city=as.data.frame(city[1:1000,2]) 
state=as.data.frame(str_match_all(each,"<state>(.*?)</state>"))
state=as.data.frame(state[1:1000,2])
country=as.data.frame(str_match_all(each,"<country>(.*?)</country>"))
country=as.data.frame(country[1:1000,2])
zip=as.data.frame(str_match_all(each,"<postalcode>(.*?)</postalcode>"))
zip=as.data.frame(zip[1:1000,2])
phone=as.data.frame(str_match_all(each,"<phone>(.*?)</phone>"))
phone=as.data.frame(phone[1:1000,2])
fax=as.data.frame(str_match_all(each,"<fax>(.*?)</fax>"))
fax=as.data.frame(fax[1:1000,2])
latitude=as.data.frame(str_match_all(each, "<latitude>([0-9.0-9]*)</latitude>"))
latitude=as.data.frame(latitude[1:1000,2])
longitude=as.data.frame(str_match_all(each,"<longitude>(-[0-9.0-9]*)</longitude>"))
longitude=as.data.frame(longitude[1:1000,2])

data=cbind(name, uid, address1, address2, city, state, country, zip, phone, fax, latitude, longitude)
assign(i, data)

}

################### add them together and make the final data

data.dennys=rbind(UT, DC, AL, HI, KS)
data.dennys=data.dennys[!duplicated(data.dennys$uid),]

#removing Dennys outside US
data.dennys=data.dennys[data.dennys$country=="US",]
#data.dennys[1693,]

# Save results as Rdata file
save(data.dennys, file="dennys/dennys_data.Rdata")



