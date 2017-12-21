meet_data = read_csv("meets.csv")
meet_data$MeetCountry = gsub('England','United Kingdom',meet_data$MeetCountry)
meet_data$MeetCountry = gsub('N.Ireland','United Kingdom',meet_data$MeetCountry)

l = length(meet_data$MeetName)

meet_data[,"x"] <- NA
meet_data[,"y"] <- NA
meet_data[,"z"] <- NA


for(row in 1:l){
  oldLat <- meet_data[row, "lat"]
  oldLong <- meet_data[row, "long"]
  #convert to radians
  oldLat <- oldLat*pi/180
  oldLong <- oldLong*pi/180
  #compute cartesian coordinates for each lat long pair
  meet_data[row,"x"] = cos(oldLat)*cos(oldLong)
  meet_data[row, "y"] = cos(oldLat)*sin(oldLong)
  meet_data[row, "z"] = sin(oldLat)
}

write.csv(meet_data, file = "meets.csv",row.names = FALSE)