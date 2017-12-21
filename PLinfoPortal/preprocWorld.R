meet_data = read_csv("meets.csv")
meet_data$MeetCountry = gsub('England','United Kingdom',meet_data$MeetCountry)
meet_data$MeetCountry = gsub('N.Ireland','United Kingdom',meet_data$MeetCountry)

#Load polygons for world map
world_spdf=readOGR( dsn= "TM_WORLD_BORDERS_SIMPL-0.3" , layer="TM_WORLD_BORDERS_SIMPL-0.3")

#Saint martin was not found in the country code package, and as no meets were held there, we changed its name to a different country
world_spdf@data$NAME = gsub('Saint Martin', 'Malta', world_spdf@data$NAME)
world_spdf@data$NAME[142] = 'Malta' 
countrycodes = countrycode(world_spdf@data$NAME,'country.name','iso3c') #doesnt recognize all countries
meetCountries = unique(meet_data$MeetCountry)

#Making sure all coutnry names are equal
for(row in 1:length(meetCountries)){ 
  index = match(countrycode(meetCountries[row],'country.name','iso3c'),countrycodes)
  world_spdf@data$NAME[index] = meetCountries[row]
}

writeOGR(world_spdf,dsn= "TM_WORLD_BORDERS_SIMPL-0.3" , layer="TM_WORLD_BORDERS_SIMPL-0.4", driver = "ESRI Shapefile")