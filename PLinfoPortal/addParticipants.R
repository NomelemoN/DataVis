rm(list = ls())
df <- read.csv("meets.csv", check.names = FALSE)
df$NumberParticipants <- vector(mode = "integer", length = nrow(df))
setwd('../meet-data')

for(row in 1:nrow(df)){
  print(as.character.factor(df[row,'MeetPath']))
  count <- length(strsplit(as.character.factor(df[row,'MeetPath']),"/")[[1]])
  print(count)
  setwd(as.character.factor(df[row,'MeetPath']))
  print(getwd())
  df2 <- read.csv('entries.csv')
  df[row,'NumberParticipants'] <- sum(!duplicated(df2$Name))
  for (i in 1:count){
    setwd('..')
  }
}
setwd("../PLinfoPortal")
write.csv(df, file = "meets.csv", row.names = FALSE)