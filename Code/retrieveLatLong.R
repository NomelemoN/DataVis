library(ggmap)
rm(list=ls())
df <- read.csv("meets.csv")

count <- 0
for(row in 1:nrow(df)){
  lat <- df[row, "lat"]
  long <- df[row, "long"]
  if((lat >= 300 || is.na(lat)) ){
    loc <- paste(df[row,"MeetCountry"], df[row,"MeetState"], df[row,"MeetTown"], sep = " ")
    place <- geocode(loc, "latlon")
    df[row, "lat"] <- place[1]
    df[row, "long"] <- place[2]
    count <- count + 1
    write.csv(df, file = "meets.csv")
  }
}
