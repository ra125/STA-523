## load required packages
source("check_packages.R")
check_packages(c("httr","XML","jsonlite","stringr"))

# Read from input file
input_file = "lq/interactive_map.html"
data = readChar(input_file, file.info(input_file)$size)

# Do parsing stuff
root = xmlRoot(htmlParse(data))
scripts = getNodeSet(root, "//script")
hotel_data_script = scripts[24]
hotel_data_script = xmlValue(hotel_data_script[[1]])                        
hotels = str_match_all(hotel_data_script, "hotelList\\.push\\((.*?)\\);")[[1]]
num_hotels = length(hotels)/2
all_fields = str_match_all(hotels,"[ |\\{}]([a-zA-Z0-9]*):")
all_fields.sorted = all_fields[[1]][,2]
len_fields = length(all_fields.sorted)
dataframe = data.frame(matrix(data=NA,nrow=num_hotels,ncol=len_fields))
for(i in 1:len_fields) {
 vals = str_match(hotels[(num_hotels+1):length(hotels)],paste("[ |\\{]", all_fields.sorted[i],":\\s*\"([^\"]*)\"",sep=""))
 dataframe[i] = vals[,2]
 attr(dataframe, "names")[i] = all_fields.sorted[i]
}

# Save results as Rdata file
save(dataframe, file="lq/list.Rdata")