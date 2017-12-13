df <- read.csv("meets.csv")
lat <- rep(300,nrow(df))
long <- rep(300,nrow(df))
df$lat <- lat
df$long <- long

write.csv(df, file = "meets.csv")

