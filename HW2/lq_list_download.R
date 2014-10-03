# Load Required packages
#source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite"))

# Download content
hotel_map_url = "www.lq.com/en/findandbook.by_interactive_map.html"
data = content(GET(hotel_map_url), as="text")

# Write
dir.create("lq/", showWarnings = FALSE)
write(data, file="lq/interactive_map.html")
