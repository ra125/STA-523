source("check_packages.R")
check_packages(c("data.table","ggmap","rgdal","maptools"))
park = fread("/home/vis/cr173/Sta523/data/parking/NYParkingViolations_small.csv",sep=",")