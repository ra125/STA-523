library('jsonlite')

jsondata <- list()
for(i in 1:nrow(original)) { 
  country <- toString(original[i, "pteam"]);
  if(is.null(jsondata[[country]])) { 
    jsondata[[country]] <- list() 
    jsondata[[country]][["Totals Goals"]] <- 0
    jsondata[[country]][["Players"]] <- list()
  }
  jsondata[[country]][["Totals Goals"]] = jsondata[[country]][["Totals Goals"]] + 1
  player <- toString(original[i, "pname"]);
  players <- jsondata[[country]][["Players"]]
  if(is.null(players[[player]])) { 
    players[[player]] <- list()
    players[[player]][["ID"]] <- toString(original[i, "pid"]);
    players[[player]][["Goals"]] <- list()
  }
  
  goal <- list(PIG=toString(original[i, "pig"]), Type=toString(original[i, "ptype"]), MatchID=toString(original[i, "mid"]), Time=as.numeric(toString(original[i, "ptime"])), ExtraTime=as.numeric(toString(original[i, "paddtime"]))) 
  
  players[[player]][["Goals"]] <- c(players[[player]][["Goals"]], list(goal))
  jsondata[[country]][["Players"]] <- players
}

final_json <- toJSON(jsondata)
prettify(final_json)