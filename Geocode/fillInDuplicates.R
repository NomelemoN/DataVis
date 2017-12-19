df <- read.csv("meetsOriginal2.csv", check.names = FALSE)
name <- character(nrow(df))
lat <- numeric(nrow(df))
long <- numeric(nrow(df))
count <- 0
for(row in 1:nrow(df)){
  if(!is.na(df[row,'lat'])){
    name[row] <- paste(df[row,'MeetCountry'],df[row, 'MeetState'], df[row,'MeetTown'], sep = " ")
    lat[row] <- df[row, "lat"]
    long[row] <- df[row, "long"]
  }else{
    a = paste(df[row,'MeetCountry'],df[row, 'MeetState'], df[row,'MeetTown'], sep = " ")
    if(a %in% name){
      index <- match(a, name)
      count <- count + 1
      df[row, "lat"] <- lat[index]
      df[row, "long"] <- long[index]
    }
  }
}
write.csv(df, file = "meetsOriginal2.csv",row.names = FALSE)