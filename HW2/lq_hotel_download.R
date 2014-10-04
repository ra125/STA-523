# Load Required packages
#source("check_packages.R")
check_packages(c("httr"))

# Load hotel info data frame
load(file="lq/list.Rdata")

dir.create("lq/hotels/", showWarnings = FALSE)

# Loop over hotels and download their pages using their specific "innNumber" in the url
num_hotels = length(dataframe[,2])
for(i in 1:num_hotels)
{
  url = paste0("http://www.lq.com/en/findandbook/hotel-details.",dataframe[i,"innNumber"],".address.html")
  page = GET(url)
  stopifnot(page$status_code == 200)
  data = content(page, as="text")
  print(url)
  write(data, file=paste0("lq/hotels/",dataframe[i,"innNumber"],".html"))
  Sys.sleep(1) # wait before grabbing the next page
}