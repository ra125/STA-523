library(devtools)
install_github("hadley/dplyr")

#cleaning the data in tax and addr to maximize the matching between them, currently only about 0.6 are matched.

#after cleaning 1st, 2nd, 3rd, and ~th, the matching goes up to 1569809
#after delete first 0 in house number, the matching goes up to 1569912

univaddr<-unique(addr$addr)
length(addr$addr)
length(univaddr)
unizaddr<-unique(z$addr)
length(unizaddr)



library("dplyr")

#not.match<-anti_join(addr$addr,z$addr)
#don't work: Error in UseMethod("anti_join") : 
#no applicable method for 'anti_join' applied to an object of class "character"

#not.mav<-anti_join(addr,tax)
not.map<-anti_join(tax,addr)
taxdt<-as.data.table(tax)
not.mav<-anti_join(addr,taxdt)

uni.not.mav<-unique(not.mav)
uni.not.map<-unique(not.map)

test<-str_detect(addr$addr, " terr")
sum(test) #249

test<-str_detect(addr$addr, " square$")
sum(test) #821

test<-str_detect(tax$addr, " square")
test[is.na(test)]<-F
sum(test) #92

test<-str_detect(addr$addr, " sq$")
sum(test) #4940


test<-str_detect(tax$addr, " sq")
test[is.na(test)]<-F
sum(test) #93

test<-str_detect(tax$addr, "circle")
test[is.na(test)]<-F
sum(test) #3

test<-str_detect(addr$addr, "cir")
test[is.na(test)]<-F
sum(test) #391

test<-str_detect(addr$addr, "cir")
test[is.na(test)]<-F
sum(test) #159

test<-str_detect(addr$addr, "place")
test[is.na(test)]<-F
sum(test) #1630

test<-str_detect(addr$addr, "pl$")
test[is.na(test)]<-F
sum(test) #30905