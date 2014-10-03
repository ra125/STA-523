load("lq/list.Rdata")

# Expand the dataframe with more fields
dataframe[,"phone"] = NA
dataframe[,"fax"] = NA
dataframe[,"floors"] = NA
dataframe[,"rooms"] = NA
dataframe[,"suites"] = NA
dataframe[,"check_in_time"] = NA
dataframe[,"check_out_time"] = NA
dataframe[,"spa"] = NA
dataframe[,"internet"] = NA
dataframe[,"pool"] = NA
dataframe[,"fitness"] = NA

# Loop over hotels
num_hotels = length(dataframe[,1])
for(i in 1:num_hotels) {
  input_file = paste0("lq/hotels/",dataframe[i, "innNumber"],".html")
  data = readChar(input_file, file.info(input_file)$size)
  dataframe[i,"phone"] = str_match(data, "(?i)Phone: ([0-9-]*)")[,2]
  dataframe[i,"fax"] = str_match(data, "(?i)Fax: ([0-9-]*)")[,2]
  dataframe[i,"floors"] = str_match(data, "(?i)Floors: ([0-9]*)\\s")[,2]
  dataframe[i,"rooms"] = str_match(data, "(?i)Rooms: ([0-9]*)\\s")[,2]
  dataframe[i,"suites"] = str_match(data, "(?i)Suites: ([0-9]*)\\s")[,2]
  dataframe[i,"check_in_time"] = str_match(data, "(?i)Check-In Time: ([0-9:]*)\\s")[,2]
  dataframe[i,"check_out_time"] = str_match(data, "(?i)Check-Out Time: ([0-9:]*)\\s")[,2]
  dataframe[i,"spa"] = str_match(data, "(?i)Spa")[,1]
  dataframe[i,"internet"] = str_match(data, "(?i)Internet access")[,1]
  dataframe[i,"pool"] = str_match(data, "(?i)Swimming pool")[,1]
  dataframe[i,"fitness"] = str_match(data, "(?i)Fitness center")[,1]
}

# Save results as Rdata file
save(dataframe, file="lq/lq_data.Rdata")