library(ggmap)
rm(list=ls())
df <- read.csv("meetsOriginal2.csv", check.names = FALSE)

for(row in 1:nrow(df)){
  lat <- df[row, "lat"]
  long <- df[row, "long"]
  if((is.na(lat)) ){
    loc <- paste(df[row,"MeetCountry"], df[row,"MeetState"], df[row,"MeetTown"], sep = " ")
    place <- geocode(loc, output = 'all', messaging = TRUE, override_limit = TRUE)
    if(place$status != "OVER_QUERY_LIMIT" && length(place$results) != 0){
      df[row, "lat"] <- place$results[[1]]$geometry$location$lat
      df[row, "long"] <- place$results[[1]]$geometry$location$lng
      write.csv(df, file = "meetsOriginal2.csv",row.names = FALSE)
    }
  }
}
