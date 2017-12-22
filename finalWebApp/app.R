library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(lattice)
library(tidyr)
library(plotly)
library(tidyverse)
library(leaflet)
library(rgdal)
library(countrycode)
library(htmltools)
library(htmlwidgets)
library(leaflet.minicharts)
library(GGally)


#=================Global Variables==================================================

# Types of federations
testedFeds <- c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF',
                'IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW',
                'THSPA','USAPL','WNPF')
# Meet Data
meet_data = read_csv("meets.csv")
meet_data$MeetCountry = gsub('England','United Kingdom',meet_data$MeetCountry)
meet_data$MeetCountry = gsub('N.Ireland','United Kingdom',meet_data$MeetCountry)

l = length(meet_data$MeetName)

meet_data$x <- 0
meet_data$y <- 0
meet_data$z <- 0


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

# Powerlifting data
lifts_data = read_csv("openpowerlifting.csv") %>%
  select(MeetID, Name, Sex, Equipment, Division, BodyweightKg, WeightClassKg,
         BestSquatKg, BestBenchKg, BestDeadliftKg, TotalKg, Wilks, Place)

# Data cleaning
lifts_data <- lifts_data[complete.cases(lifts_data),]

lifts_data$Place = as.integer(lifts_data$Place)
lifts_data$WeightClassKg <- as.factor(lifts_data$WeightClassKg)

popular_weight_classes = lifts_data %>% group_by(WeightClassKg) %>% summarise(n = n()) %>%
  filter(n>1000)

lifts_data = filter(lifts_data, BestSquatKg > 0 & BestBenchKg > 0 & 
                      BestDeadliftKg > 0 & Place > 0 & Place < 6 &
                      WeightClassKg %in% popular_weight_classes$WeightClassKg) %>%
  distinct(MeetID, Name, Sex, Equipment, Division, BodyweightKg, WeightClassKg,
           BestSquatKg, BestBenchKg, BestDeadliftKg, TotalKg, Wilks, Place) %>%
  inner_join(select(meet_data, MeetID, Year, Federation)) %>%
  mutate(fed_type = ifelse(Federation %in% testedFeds, "Tested", "Untested"), 
         cons_val = 1, color_var = 1, size_var = 1)

lifts_data$Year <- as.factor(lifts_data$Year)
lifts_data$Federation <- as.factor(lifts_data$Federation)
lifts_data$fed_type <- as.factor(lifts_data$fed_type)

# Fed and category lists
weight_categories <- as.vector(popular_weight_classes$WeightClassKg)
allFeds <- sort(as.vector(unique(lifts_data$Federation)))
untestedFeds <- setdiff(allFeds, testedFeds)
genders = unique(lifts_data$Sex)
equipments = unique(lifts_data$Equipment)

#=============================Preprocessing for Leaflet maps====================================

#Load polygons for world map
world_spdf=readOGR( dsn= "TM_WORLD_BORDERS_SIMPL-0.3" , layer="TM_WORLD_BORDERS_SIMPL-0.3")

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

#=================UI Design========================================================
ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = 'Powerlifting Info Portal', titleWidth = 250),
                    
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       menuItem("Geograpphical Shifts", tabName = "popularity", icon = icon("globe")),
                                       menuItem("Effect of Drugs", tabName = "drugs", icon = icon("rocket")),
                                       menuItem("Score Trends", tabName = "strategy", icon = icon("line-chart")),
                                       menuItem("About Us", tabName = "profile", icon = icon("address-card-o"))
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "popularity",
                                
                                h2("Geograpghical Shifts"), 
                                sliderInput("pop_year", "Year", 
                                            min = min(meet_data$Year), max = max(meet_data$Year), step = 1,
                                            value = max(meet_data$Year), animate = animationOptions(interval = 3000, loop = FALSE)),
                                
                                br(),
                                
                                checkboxGroupInput("pop_feds", "Choose Federations:", allFeds, inline = TRUE),
                                checkboxGroupInput('pop_fed_type','Types', c('Tested', 'Untested'), inline = TRUE),
                                selectInput("pop_countries", "Country", choices = union("All",meetCountries), label = "Countries", multiple = FALSE),
                                br(), 
                                
                                fluidRow(
                                  box(width = 12, title = "Meets across the world",
                                      leafletOutput("intensity_map") 
                                  )
                                ),
                                br(),
                                sliderInput("pop_yearRange", "Range of Years",
                                            min = min(meet_data$Year, na.rm = TRUE), max = max(meet_data$Year, na.rm = TRUE), step = 1,
                                            value = c(min(meet_data$Year, na.rm = TRUE), max(meet_data$Year, na.rm = TRUE))),
                                br(),
                                fluidRow(
                                  box(width = 12, title = "Meet locations averaged across Years",
                                      leafletOutput("vector_map")
                                  )
                                )
                        ),
                        tabItem(tabName = "drugs",
                                h2("Tested vs Untested"), 
                                checkboxGroupInput('drug_equipment','Equipment', equipments, inline = TRUE),
                                br(),
                                sliderInput("drug_bodyweight", "Bodyweight Range",
                                            min = min(lifts_data$BodyweightKg, na.rm = TRUE), max = max(lifts_data$BodyweightKg, na.rm = TRUE), step = 1,
                                            value = c(min(lifts_data$BodyweightKg, na.rm = TRUE), max(lifts_data$BodyweightKg, na.rm = TRUE))),
                                br(),
                                fluidRow(
                                  box(width = 6,plotlyOutput("parcoord")),
                                  box(width = 6, title = "Performance increase of Untested over Tested", plotlyOutput("barplot"))
                                )
                        ),
                        tabItem(tabName = "strategy",
                                
                                fluidRow(
                                  column(width = 9,
                                         tags$head(
                                           tags$style(HTML("
                                            #final_text {
                                              text-align: center;
                                            }
                                            div.box-header {
                                              text-align: center;
                                            }
                                            ")
                                           )
                                         ),
                                         
                                         box(width = 12, height = 620,status = "primary", 
                                             title = "Powerlifting scores for top rankers",
                                             plotlyOutput("scatter_3d")
                                         )
                                  ),
                                  column(width = 3,
                                         box(width = 12, title = "Data filters", status = "info",
                                             
                                             
                                             selectInput("top_category", "Weight class:", weight_categories),
                                             selectInput("top_sex", "Sex:", c("All", genders)),
                                             selectInput("top_equipment", "Equipment:", c("All", equipments)),
                                             selectInput("top_federation", "Federation:", c("All", "Tested", "Untested", allFeds))
                                         ),
                                         br(),
                                         #MeetID, Name, Sex, Equipment, Division, BodyweightKg, WeightClassKg,
                                         #BestSquatKg, BestBenchKg, BestDeadliftKg, TotalKg,  Wilks, Place, 
                                         #Year, Federation, fed_type, cons_val, color_var, size_var
                                         box(width = 12, title = "Plot controls",status = "info",
                                             selectInput("col_var", "Bubble color variable:", c("Sex"=3,
                                                                                                "Equipment"=4,
                                                                                                "Federation"=15,
                                                                                                "Federation type"=16,
                                                                                                "Year"=14,
                                                                                                "Total kg"=11,
                                                                                                "Wilks"=12
                                             )),
                                             selectInput("size_var", "Bubble size variable:", c("None"=17,
                                                                                                "Total kg"=11,
                                                                                                "Wilks"=12,
                                                                                                "Bodyweight"=6))
                                         )
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "profile",
                                p("This app has been developed for Data Visualization course at TU Delft.")
                        )
                      )
                    )
)

#==============Logic for interactive stuff goes into server function==================

server <- function(input, output) {
  
  output$scatter_3d <- renderPlotly({
    
    sq_data = filter(lifts_data, WeightClassKg == input$top_category)
    if (input$top_sex!="All")
      sq_data = filter(lifts_data, Sex == isolate(input$top_sex))
    if (input$top_equipment!="All")
      sq_data = filter(lifts_data, Equipment == isolate(input$top_equipment))
    if (input$top_federation=="Tested")
      sq_data = filter(lifts_data, fed_type == "Tested")
    if (input$top_federation=="Untested")
      sq_data = filter(lifts_data, fed_type == "Unested")
    if (input$top_federation!="All"&&input$top_federation!="Tested"&&input$top_federation!="Untested")
      sq_data = filter(lifts_data, Federation == isolate(input$top_federation))
    sq_data$color_var = sq_data[[3]]
    sq_data$size_var = sq_data[[14]]
    if(!is.na(input$col_var) && !is.na(input$size_var)) {
      sq_data$color_var = sq_data[[as.integer(isolate(input$col_var))]]
      sq_data$size_var = sq_data[[as.integer(isolate(input$size_var))]]
    }
    
    colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
    p <- plot_ly(sq_data, x = ~BestSquatKg, y = ~BestBenchKg, z = ~BestDeadliftKg, 
                 color = ~color_var,  colors = colors, size = ~size_var,
                 marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 5),
                 text = ~paste('Total (Kg):', TotalKg,'<br>Name:', Name, #'<br>Sex:', Sex, 
                               '<br>Bodyweight (kg):', BodyweightKg, '<br>Wilks:', Wilks,
                               #'<br>Equipment:', Equipment,
                               '<br>Meet Id:', MeetID,
                               '<br>Place:', Place)) %>%
      layout(#title = 'Powerlifting scores',
        width = 850,
        height = 550,
        scene = list(xaxis = list(title = 'Squat',
                                  #gridcolor = 'rgb(255, 255, 255)',
                                  #range = c(2.003297660701705, 5.191505530708712),
                                  #type = 'log',
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                     yaxis = list(title = 'Benchpress',
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                     zaxis = list(title = 'Deadlift',
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2)),
        paper_bgcolor = 'rgb(255, 255, 255)',
        plot_bgcolor = 'rgb(243, 243, 243)'
      )
    p
  })
  
  
  ## pop_year, pop_feds, pop_fed_type, pop_countries, pop_yearRange
  myYear <- reactive({
    if(!is.null(input$pop_year))
      input$pop_year
    else
      2017
  })
  
  ## Return selected federations in Tab 1
  myFeds <- reactive({
    if(!is.null(input$pop_feds))
      input$pop_feds
    else
      allFeds
  })
  
  ## Return selected countries in Tab 1
  myCountry <- reactive({
    if(!is.null(input$pop_countries) && !(input$pop_countries == "All")){
      input$pop_countries
    }
    else
      meetCountries
  })
  
  ## Return all feds of selected type in Tab 1
  type <-reactive({
    if(!is.null(input$pop_fed_type)){
      if("Tested" %in% input$pop_fed_type && "Untested" %in% input$pop_fed_type){
        allFeds
      }else if("Tested" %in% input$pop_fed_type){
        testedFeds
      }else if("Untested" %in% input$pop_fed_type){
        untestedFeds
      }
    }else{
      allFeds
    }
  })
  
  #Year range used in Tab 1
  min_year <- reactive({
    if(!is.null(input$pop_yearRange)){ 
      input$pop_yearRange[1]
    }
    else
      min(meet_data$Year, na.rm = TRUE)
  })
  
  max_year <- reactive({
    if(!is.null(input$pop_yearRange)){ 
      input$pop_yearRange[2]
    }
    else
      min(meet_data$Year, na.rm = TRUE)
  })
  
  
  #This is the logic for the intensity_map shown in tab 1, Powerlifting across the world
  output$intensity_map <- renderLeaflet({
    
    #Filters meets for the selected year, federations, and type
    dataset <- filter(
      meet_data, 
      Year==myYear(), 
      Federation %in% myFeds(), 
      Federation %in% type(), 
      MeetCountry %in% myCountry()) %>%
      #Counts the number of meet for each country
      count(MeetCountry) 
    
    #same as above, but without counting the number of countries
    meets <- filter(
      meet_data, 
      Year==myYear(), 
      Federation %in% myFeds(), 
      Federation %in% type(), 
      MeetCountry %in% myCountry())
    
    #We add a new data column to the world map data which holds the number of meets in a country
    world_spdf@data$n = 0
    if(nrow(dataset) != 0){
      for(row in 1:nrow(dataset)){
        #If country names are the same between the meet dataset and the world map, add meetnumber to the country column in the same index 
        index = match(dataset[[row, "MeetCountry"]], world_spdf@data$NAME)
        world_spdf@data$n[index] = dataset[[row, "n"]]
      }
    }
    
    #colors
    mybins=c(0,1,10,100,200,400,800,1600) #set up color bins
    mypalette = colorBin( palette="Oranges", domain=world_spdf@data$n, bins=mybins) #choose color palette to base on
    
    #country title
    mytext=paste("<b>Country: </b>", world_spdf@data$NAME,"<br/>", "<b>Number of Meets: </b>", world_spdf@data$n, sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    if(length(meets[["MeetName"]]) != 0){ #There are meets
      
      leaflet(world_spdf) %>% 
        #setView(lat = 53.0000, lng = 9.0000, zoom = 3) %>% #europe
        
        addTiles(options = tileOptions(noWrap = TRUE))  %>% #dont show the world map repeated
        addPolygons( #add selectable country polygons
          fillColor = ~mypalette(n), stroke=TRUE, fillOpacity = 0.7, color="white", weight=0.4, #color the country polygons based on meet nmber
          highlight = highlightOptions( weight = 5, color = ~colorNumeric("Oranges", n)(n), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE), #highlight options
          label = mytext, #show the country text
          labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 7px"), textsize = "13px", direction = "auto") #specify label options
        ) %>%
        
        #add individual meet datapoints
        addMarkers(
          data = meets, 
          ~long, ~lat, 
          popup = ~as.character(paste("<b> Number of participants: </b>", NumberParticipants, "<b> Date: </b>", Date, "<b> MeetCountry: </b>", MeetCountry, "<b> MeetState: </b>", MeetState, "<b> MeetTown: </b>", MeetTown, sep = "<br>")), 
          label =~as.character(MeetName), 
          clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
        ) %>%
        
        #Add legend for the colors of individual countries
        addLegend( pal=mypalette, values=~n, opacity=0.9, title = paste("Number of Meets in",myYear(), sep = "<br>"), position = "bottomleft" )
      
    }else{ #Draw map without markers if there are none present
      
      leaflet(world_spdf) %>% 
        #setView(lat = 53.0000, lng = 9.0000, zoom = 3) %>% #europe
        addTiles(options = tileOptions(noWrap = TRUE))  %>% 
        addPolygons( 
          fillColor = ~mypalette(n), stroke=TRUE, fillOpacity = 0.7, color="white", weight=0.4,
          highlight = highlightOptions( weight = 5, color = ~colorNumeric("Oranges", n)(n), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
          label = mytext,
          labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 7px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~n, opacity=0.9, title = paste("Number of Meets in",myYear(), sep = "<br>"), position = "bottomleft" )
      
    }
    
  })
  #This is the logic for the vector map shown in Tab 1, showing geographic shift over time
  output$vector_map <- renderLeaflet({
    dataset <- filter(meet_data, Federation %in% myFeds(), Federation %in% type(), Year >= min_year(), Year <= max_year(), MeetCountry %in% myCountry()) #Filters by Federation and type of meet
    
    lats <- vector(mode = "numeric", length = max(dataset$Year) - min(dataset$Year))
    longs <- vector(mode = "numeric", length = max(dataset$Year) - min(dataset$Year))
    
    years = min(dataset$Year):max(dataset$Year)
    
    #Compute the average location of powerlifting meets per year
    for(year in years){
      yearDataset <- filter(dataset, Year == year)
      
      totWeight = length(yearDataset$MeetName)
      totalx = sum(yearDataset$x)
      totaly = sum(yearDataset$y)
      totalz = sum(yearDataset$z)
      
      avgx <- totalx/totWeight
      avgy <- totaly/totWeight
      avgz <- totalz/totWeight
      
      
      #convert back to longitude and latitude
      newLong <- atan2(avgy,avgx)
      hyp <- sqrt(avgx*avgx+avgy*avgy)
      newLat <- atan2(avgz,hyp)
      
      #convert back to degrees
      newLat <- newLat*180/pi
      newLong <- newLong*180/pi
      
      lats[year + 1 - min(dataset$Year)] = newLat
      longs[year + 1 - min(dataset$Year)] = newLong
    }
    
    
    #create a dataframe containing all lat and long values, without NaN values
    loc.data <- data.frame(lats, longs, years)
    loc.data <- loc.data[is.finite(loc.data$lats),]
    
    #easier way to work with for the addFlows() function
    latLength <- length(loc.data$lats)
    print(latLength)
    
    if(latLength > 1){
      
      #Create a dataframe containing for columns: lats1 (old latitude values), lats2 (new latitude values), longs1 (old longitude values), and longs2 (new longitude values)
      loc.data2 <- data.frame(lats1 = loc.data$lats[1:latLength-1], lats2 = loc.data$lats[2:latLength], longs1 = loc.data$longs[1:latLength-1], longs2 = loc.data$longs[2:latLength])
      #define the two color extremes over which the year circles run
      colors = colorRampPalette(c("yellow",'red'))
      
      #The actual map
      leaflet(world_spdf)%>%
        
        #Dont show the worldmap repeatedly
        addTiles(options = tileOptions(noWrap = TRUE)) %>%
        
        #Add the vectors show in the map
        addFlows(
          lng0 = loc.data2$longs1,lat0 = loc.data2$lats1, lng1 = loc.data2$longs2, lat1 = loc.data2$lats2,
          flow = 1,
          popup =  popupArgs(noPopup = TRUE),
          maxThickness = 3,
          color = "orange",
          opacity = 0.5
        ) %>%
        #Show all averaged meet locations on the world map
        addMinicharts(
          loc.data$longs, loc.data$lats,
          chartdata = loc.data$years,
          showLabels = TRUE,
          labelText = as.character(loc.data$years),
          width = 25,
          fillColor = colors(length(loc.data$years)),
          opacity = 0.5
        ) %>%
        
        #Add vector between starting year and final year
        addFlows(
          lng0 = loc.data2$longs1[1], lat0 = loc.data2$lats1[1], lng1 = loc.data2$longs2[length(loc.data2$longs2)], lat1 = loc.data2$lats2[length(loc.data2$lats2)],
          popup =  popupArgs(noPopup = TRUE),
          maxThickness = 5,
          color = "purple",
          opacity = 1
        ) %>%
        
        #Show the starting year with greater size and color
        addMinicharts(
          loc.data$longs[1], loc.data$lats[1],
          chartdata = loc.data$years[1],
          showLabels = TRUE,
          labelText = as.character(loc.data$years[1]),
          width = 30,
          fillColor = "yellow",
          opacity = 1
        ) %>%
        
        #show the final year with greater size and color
        addMinicharts(
          loc.data$longs[length(loc.data$years)], loc.data$lats[length(loc.data$years)],
          chartdata = loc.data$years[length(loc.data$years)],
          showLabels = TRUE,
          labelText = as.character(loc.data$years[length(loc.data$years)]),
          width = 30,
          fillColor = "red",
          opacity = 1
        ) 
    }else{
      leaflet(world_spdf)%>%
        #Dont show the worldmap repeatedly
        addTiles(options = tileOptions(noWrap = TRUE))
    }
    
  })
  
  # returns selected equipments in Tab 2
  equipment_input <- reactive({
    if(!is.null(input$drug_equipment))
      input$drug_equipment
    else
      equipments
  })
  
  # returns selected min bodyweight for range in Tab 2
  bodyweight_input_min <- reactive({
    if(!is.null(input$drug_bodyweight)){ 
      input$drug_bodyweight[1]
    }
    else
      min(lifts_data$BodyweightKg)
  })
  
  # returns selected max bodyweight for range in Tab 2
  bodyweight_input_max <- reactive({
    if(!is.null(input$drug_bodyweight)){
      input$drug_bodyweight[2]
    }
    else
      max(lifts_data$BodyweightKg)
  })
  
  output$barplot <- renderPlotly({
    #untested data
    ud <- filter(
      lifts_data,
      fed_type == "Untested",
      Equipment %in% equipment_input(), 
      BodyweightKg <= bodyweight_input_max(), 
      BodyweightKg >= bodyweight_input_min()
    )
    
    td <- filter(
      lifts_data,
      fed_type == "Tested",
      Equipment %in% equipment_input(), 
      BodyweightKg <= bodyweight_input_max(), 
      BodyweightKg >= bodyweight_input_min()
    )
    
    #untested male
    um <- filter(
      ud, 
      Sex == "M" 
    )
    
    #tested male
    tm <- filter(
      td, 
      Sex == "M"
    )
    
    #untested female
    uf <- filter(
      ud, 
      Sex == "F"
    )
    
    #tested female
    tf <- filter(
      td, 
      Sex == "F"
    )
    
    #averages for  category
    
    s1 <- "BestSquatKg"
    s2 <- "BestBenchKg"
    s3 <- "BestDeadliftKg"
    s4 <- "TotalKg"
    s5 <- "Wilks"
    titles <- c(s1,s2,s3,s4,s5)
    
    squat <- c(
      lapply(um[,s1], mean)[[1]], 
      lapply(tm[,s1], mean)[[1]],
      lapply(uf[,s1], mean)[[1]],
      lapply(tf[,s1], mean)[[1]]
    )
    bench <- c(
      lapply(um[,s2], mean)[[1]], 
      lapply(tm[,s2], mean)[[1]],
      lapply(uf[,s2], mean)[[1]],
      lapply(tf[,s2], mean)[[1]]
    )
    deadlift <- c(
      lapply(um[,s3], mean)[[1]], 
      lapply(tm[,s3], mean)[[1]],
      lapply(uf[,s3], mean)[[1]],
      lapply(tf[,s3], mean)[[1]]
    )
    
    total <- c(
      lapply(um[,s4], mean)[[1]], 
      lapply(tm[,s4], mean)[[1]],
      lapply(uf[,s4], mean)[[1]],
      lapply(tf[,s4], mean)[[1]]
    )
    
    wilks <- c(
      lapply(um[,s5], mean)[[1]], 
      lapply(tm[,s5], mean)[[1]],
      lapply(uf[,s5], mean)[[1]],
      lapply(tf[,s5], mean)[[1]]
    )
    
    groups <- c(
      "Untested Male",
      "Tested Male", 
      "Untested Female", 
      "Tested Female"
    )
    
    data_all <- data.frame(Squat = squat,Bench = bench, Deadlift = deadlift,Total = total, Wilks = wilks, Groups = groups)
    
    
    x <- c("Squat", "Bench", "Deadlift", "Total", "Wilks")
    y <- (as.numeric(data_all[1,1:5])/as.numeric(data_all[2,1:5]) - 1)*100
    y2 <- (as.numeric(data_all[3,1:5])/as.numeric(data_all[4,1:5]) - 1)*100
    per_data <- data.frame(x,Men = y,Women = y2)
    order <- list(categoryorder = "array", categoryarray = x)
    
    per_data %>% 
      plot_ly() %>%
      add_trace(x = ~x, y = ~y, type = 'bar',  textposition = 'auto',
                marker = list(color = "#5268ea",
                              line = list(color = "#5268ea", width = 1.5)), name = "Men") %>%
      add_trace(x = ~x, y = ~y2, type = 'bar',
                marker = list(color = "#d33937",
                              line = list(color = "#d33937", width = 1.5)), name = "Women") %>%
      layout(
        barmode = 'group',
        yaxis = list(title = "%"),
        xaxis = order)
  })
  
  #This is the logic for the parallel coordinate plot in Tab 2  
  output$parcoord <- renderPlotly({
    #untested data
    ud <- filter(
      lifts_data,
      fed_type == "Untested",
      Equipment %in% equipment_input(), 
      BodyweightKg <= bodyweight_input_max(), 
      BodyweightKg >= bodyweight_input_min()
    )
    
    td <- filter(
      lifts_data,
      fed_type == "Tested",
      Equipment %in% equipment_input(), 
      BodyweightKg <= bodyweight_input_max(), 
      BodyweightKg >= bodyweight_input_min()
    )
    
    #untested male
    um <- filter(
      ud, 
      Sex == "M" 
    )
    
    #tested male
    tm <- filter(
      td, 
      Sex == "M"
    )
    
    #untested female
    uf <- filter(
      ud, 
      Sex == "F"
    )
    
    #tested female
    tf <- filter(
      td, 
      Sex == "F"
    )
    
    #averages for  category
    
    s1 <- "BestSquatKg"
    s2 <- "BestBenchKg"
    s3 <- "BestDeadliftKg"
    s4 <- "TotalKg"
    s5 <- "Wilks"
    titles <- c(s1,s2,s3,s4,s5)
    
    squat <- c(
      lapply(um[,s1], mean)[[1]], 
      lapply(tm[,s1], mean)[[1]],
      lapply(uf[,s1], mean)[[1]],
      lapply(tf[,s1], mean)[[1]]
    )
    bench <- c(
      lapply(um[,s2], mean)[[1]], 
      lapply(tm[,s2], mean)[[1]],
      lapply(uf[,s2], mean)[[1]],
      lapply(tf[,s2], mean)[[1]]
    )
    deadlift <- c(
      lapply(um[,s3], mean)[[1]], 
      lapply(tm[,s3], mean)[[1]],
      lapply(uf[,s3], mean)[[1]],
      lapply(tf[,s3], mean)[[1]]
    )
    
    total <- c(
      lapply(um[,s4], mean)[[1]], 
      lapply(tm[,s4], mean)[[1]],
      lapply(uf[,s4], mean)[[1]],
      lapply(tf[,s4], mean)[[1]]
    )
    
    wilks <- c(
      lapply(um[,s5], mean)[[1]], 
      lapply(tm[,s5], mean)[[1]],
      lapply(uf[,s5], mean)[[1]],
      lapply(tf[,s5], mean)[[1]]
    )
    
    groups <- c(
      "Untested Male",
      "Tested Male", 
      "Untested Female", 
      "Tested Female"
    )
    
    data_all <- data.frame(Squat = squat,Bench = bench, Deadlift = deadlift,
                           Total = total, Wilks = wilks, Groups = groups)
    
    
    #create the parallel coordinate plot
    ggparcoord(data_all, columns = 1:(ncol(data_all)-1), groupColumn = ncol(data_all),
               scale = "globalminmax", alphaLines = 1) + 
      geom_line(size = 0.5)  +
      ggtitle("Powerlifting performance between groups") + 
      xlab("Dimensions") + ylab("Kilos") + 
      scale_colour_manual(values = c("Untested Male" = "#5268ea", 
                                     "Tested Male" = "#60beea", 
                                     "Untested Female" = "#d33937",
                                     "Tested Female" = "#eaa86e"))
    
  })
  
}

#==============Run the application (no change needed here)============================

shinyApp(ui = ui, server = server)

