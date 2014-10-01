source("check_packages.R")
check_packages(c("httr","XML","stringr","jsonlite"))
hotel_map_url = "www.lq.com/en/findandbook.by_interactive_map.html"
data = content(GET(hotel_map_url), as="text")
write(data, file="html/interactive_map.html")


root = xmlRoot(htmlParse(data))
scripts = getNodeSet(root, "//script")
hotel_data_script = scripts[24]
hotel_data_script = xmlValue(hotel_data_script[[1]]
                             hotels = str_match_all(hotel_data_script, "hotelList\\.push\\((.*?)\\);")[[1]]
                             num_hotels = length(hotels)/2
                             all_fields = str_match_all(p,"[ |\\{}]([a-zA-Z0-9]*):")
                             all_fields.sorted = all_fields[[1]][,2]
                             len_fields = length(all_fields.sorted)
                             dataframe = data.frame(matrix(data=NA,nrow=num_hotels,ncol=len_fields))
                             for(i in 1:len_fields) {
                               vals = str_match(hotels[(num_hotels+1):length(hotels)],paste("[ |\\{]", all_fields.sorted[i],":\\s*\"([^\"]*)\"",sep=""))
                               dataframe[i] = vals[,2]
                               attr(dataframe, "names")[i] = all_fields.sorted[i]
                             })
                             