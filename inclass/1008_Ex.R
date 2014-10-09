### This relies on C, which consumes a lot of RAM. Save your work as you go.

library(data.table)
install.packages("dplyr")
library(dplyr)
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations.csv",sep=",")
head(park)

str(transmute(park, dt=paste(Issue.Date,Violation.Time)) %>% mutate(dt=ymd_hm(dt)) 
    %>% mutate(hour=hour(dt), wday=as.character(wday(dt,late=TRUE ))) )

date=(transmute(park, dt=paste(Issue.Date,Violation.Time)) %>% mutate(dt=ymd_hm(dt)) 
      %>% mutate(hour=hour(dt), wday=as.character(wday(dt,late=TRUE )))
      
group_by(date, wday) %>% summarize(n())

group_by(date, hour) %>% summarize(n())

group_by(date, hour, wday) %>% summarize(n())

which.max(hourwday$`n()`)

    precinct<-park$"Violation Location"
house_street<-cbind(park$"House Number", park$"Street Name")
address<-apply(house_street,1,paste,collapse=" ")

