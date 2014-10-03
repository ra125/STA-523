
## load required packages

check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages(c("httr","XML","sp","stringr","jsonlite","rgeos","maptools", "rgdal","ggplot2","spatsta","stringr"))



dir.create("dennys", showWarnings = FALSE)
key="8D6F0428-F3A9-11DD-8BF2-659237ABAA09"

###### states to go through!
names=c("UT","DC","KS","AL","HI")



################ Looping

for(i in names) {
  
  name=i
  url = paste0("http://hosted.where2getit.com/dennys/ajax?&xml_request=%3Crequest%3E%3Cappkey%3E",
               key,
               "%3C%2Fappkey%3E%3Cformdata+id%3D%22locatorsearch%22%3E%3Cdataview%3Estore_default%3C%2Fdataview%3E%3Climit%3E3000%3C%2Flimit%3E%3Cgeolocs%3E%3Cgeoloc%3E%3Caddressline%3E,%20",
               name,
               "%3C%2Faddressline%3E%3Clongitude%3E%3C%2Flongitude%3E%3Clatitude%3E%3C%2Flatitude%3E%3C%2Fgeoloc%3E%3C%2Fgeolocs%3E%3Csearchradius%3E3000|10000%3C%2Fsearchradius%3E%3C%2Fformdata%3E%3C%2Frequest%3E")   
  
  d=GET(url)
  stopifnot(d$status_code == 200)
  s = content(d, as="text")
  # Save the file locally
  file = paste0("dennys/",name,".xml")
  write(s, file=file)
  
}



