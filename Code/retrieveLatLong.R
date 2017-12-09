library(ggmap)
rm(list=ls())
df <- read.csv("meets.csv", check.names = FALSE)

for(row in 1:nrow(df)){
  lat <- df[row, "lat"]
  long <- df[row, "long"]
  if((lat >= 300 || is.na(lat)) ){
    loc <- paste(df[row,"MeetCountry"], df[row,"MeetState"], df[row,"MeetTown"], sep = " ")
    #place <- geocode(loc, "latlon")
    #df[row, "lat"] <- place[1]
    #df[row, "long"] <- place[2]
    df[row, "lat"] <- runif(1,-90.0,90.0)
    df[row, "long"] <- runif(1,-180.0,180.0)
  }
}
write.csv(df, file = "meets.csv",row.names = FALSE)