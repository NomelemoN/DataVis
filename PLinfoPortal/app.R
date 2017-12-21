
#Firstly install below libraries from the packages tab in lower left pane

library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(countrycode)
library(htmltools)
library(htmlwidgets)
library(leaflet.minicharts)
library(plotly)
library(GGally)
#=================Global Variables==================================================


rm(list=ls())
## Meet Data
meet_data = read_csv("meets.csv")
meet_data$MeetCountry = gsub('England','United Kingdom',meet_data$MeetCountry)
meet_data$MeetCountry = gsub('N.Ireland','United Kingdom',meet_data$MeetCountry)

###oyku
lifter_data <- read.csv(file = 'modified.csv', header = TRUE, sep = ',')
lifter_data <- filter(lifter_data, 
        !is.na(BestBenchKg), 
        !is.na(BestSquatKg), 
        !is.na(BestDeadliftKg), 
        !is.na(TotalKg)
)

distribution <- function(inputColumn){
  d <- density(na.omit(abs(inputColumn))) # returns the density distribution
  return(d)
}

#Types of federations
testedFeds <- c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF')
allFeds <- c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO', 'FFForce','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF')
untestedFeds <- setdiff(allFeds, testedFeds)

testedData <- filter(lifter_data, Federation %in% testedFeds)
untestedData <- filter(lifter_data, Federation %in% untestedFeds)

# to use for check boxes
genders = unique(lifter_data$Sex)
equipments = unique(lifter_data$Equipment)

#Load polygons for world map
world_spdf=readOGR( dsn= "TM_WORLD_BORDERS_SIMPL-0.3" , layer="TM_WORLD_BORDERS_SIMPL-0.4")

meetCountries = unique(meet_data$MeetCountry)
#=================UI Design========================================================
ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = 'Powerlifting Info Portal', titleWidth = 250),
                    
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       menuItem("Growing Popularity", tabName = "popularity", icon = icon("globe")),
                                       menuItem("Effect of Drugs", tabName = "drugs", icon = icon("rocket")),
                                       menuItem("Exploring Strategies", tabName = "strategy", icon = icon("balance-scale")),
                                       menuItem("About Us", tabName = "profile", icon = icon("address-card-o"))
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "popularity",
                                
                                #Static content and layout for for both interactive and 
                                #static stuff on a tab defined here
                                
                                h2("Powerlifting is gaining popularity worldwide"), ##Add this text in second level heading format
                                
                                sliderInput("year", "Year", ##Create a input element
                                            min = min(meet_data$Year), max = max(meet_data$Year), step = 1,
                                            value = max(meet_data$Year), animate = animationOptions(interval = 3000, loop = FALSE)),
                                
                                br(), ## Add a break line
                                
                                checkboxGroupInput("feds", "Choose Federations:", allFeds, inline = TRUE),
                                checkboxGroupInput('type','Types', c('Tested', 'Untested'), inline = TRUE),
                                selectInput("countries", "Country", choices = union("All",meetCountries), label = "Countries",multiple = FALSE),
                                br(), 
                                
                                fluidRow( 
                                  #row of content (full screen width is 12)
                                  
                                  box(width = 12, title = "Meets across the world",
                                      
                                      leafletOutput("intensity_map") 
                                      
                                  )
                                ),
                                
                                br(),
                                sliderInput("yearRange", "Range of Years",
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
                                checkboxGroupInput('equipment','Equipment', equipments, inline = TRUE),
                                br(),
                                sliderInput("bodyweight", "Bodyweight Range",
                                            min = min(lifter_data$BodyweightKg, na.rm = TRUE), max = max(lifter_data$BodyweightKg, na.rm = TRUE), step = 1,
                                            value = c(min(lifter_data$BodyweightKg, na.rm = TRUE), max(lifter_data$BodyweightKg, na.rm = TRUE))),
                                br(),
                                fluidRow(
                                  box(width = 12,plotlyOutput("parcoord")
                                  )
                                 )#,
                                # br(),
                                # selectInput("lift_kg", c("Total", "Squat", "Bench", "Deadlift"), label = "Lifts", multiple = FALSE),
                                # checkboxGroupInput('gender','Gender', genders, inline = TRUE),
                                # br(),
                                # fluidRow(
                                #   box(width = 12, title = "Comparison between tested and untested federations",
                                #       plotOutput("drugs_comparison")
                                #   )
                                # )
                        ),
                        tabItem(tabName = "strategy",
                                p("Asdfgh")
                        ),
                        tabItem(tabName = "profile",
                                p("This app has been developed for Data Visualization course at TU Delft.")
                        )
                      )
                    )
)

#==============Logic for interactive stuff goes into server function==================

server <- function(input, output) {
  
  ## Return the year selected on the input slider in Tab 1
  myYear <- reactive({
    if(!is.null(input$year))
      input$year
    else
      2017
  })
  
  ## Return selected federations in Tab 1
  myFeds <- reactive({
    if(!is.null(input$feds))
      input$feds
    else
      allFeds
  })
  
  ## Return selected countries in Tab 1
  myCountry <- reactive({
    if(!is.null(input$countries) && !(input$countries == "All")){
      input$countries
    }
    else
      meetCountries
  })
  
  ## Return all feds of selected type in Tab 1
  type <-reactive({
    if(!is.null(input$type)){
      if("Tested" %in% input$type && "Untested" %in% input$type){
        allFeds
      }else if("Tested" %in% input$type){
        testedFeds
      }else if("Untested" %in% input$type){
        untestedFeds
      }
    }else{
      allFeds
    }
  })
  
  #Year range used in Tab 1
  min_year <- reactive({
    if(!is.null(input$yearRange)){ 
      input$yearRange[1]
    }
    else
      min(meet_data$Year, na.rm = TRUE)
  })
  
  max_year <- reactive({
    if(!is.null(input$yearRange)){ 
      input$yearRange[2]
    }
    else
      min(meet_data$Year, na.rm = TRUE)
  })
  
  ## Return selected lift type to show at plot in Tab 2
  lift_kg_input <- reactive({
    if(!is.null(input$lift_kg)){
      if("Total" %in% input$lift_kg){
        28
      }else if("Squat" %in% input$lift_kg){
        17
      }else if("Bench" %in% input$lift_kg){
        22
      }else if("Deadlift" %in% input$lift_kg){
        27
      }
    }
    else
      28
  })
  
  # returns selected equipments in Tab 2
  equipment_input <- reactive({
    if(!is.null(input$equipment))
      input$equipment
    else
      equipments
  })
  
  # returns selected gender in Tab 2
  gender_input <- reactive({
    if(!is.null(input$gender))
      input$gender
    else
      genders
  })
  
  # returns selected min bodyweight for range in Tab 2
  bodyweight_input_min <- reactive({
    if(!is.null(input$bodyweight)){ 
      input$bodyweight[1]
    }
    else
      min(lifter_data$BodyweightKg, na.rm = TRUE)
  })
  
  # returns selected max bodyweight for range in Tab 2
  bodyweight_input_max <- reactive({
    if(!is.null(input$bodyweight)){
      input$bodyweight[2]
    }
    else
      max(lifter_data$BodyweightKg, na.rm = TRUE)
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
      print(lats)
      print(longs)
      print(years)
      loc.data <- data.frame(lats, longs, years)
      loc.data <- loc.data[is.finite(loc.data$lats),]
      
      #easier way to work with for the addFlows() function
      latLength <- length(loc.data$lats)
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
  
  # output$drugs_comparison <- renderPlot({
  # 
  #   # filters data as tested vs untested
  #   data_untested <- filter(
  #     untestedData,
  #     Sex %in% gender_input(),
  #     Equipment %in% equipment_input(),
  #     BodyweightKg <= bodyweight_input_max(),
  #     BodyweightKg >= bodyweight_input_min(),
  #     !is.na(BestBenchKg),
  #     !is.na(BestSquatKg),
  #     !is.na(BestDeadliftKg),
  #     !is.na(TotalKg)
  #     )
  #   data_tested <- filter(
  #     testedData,
  #     Sex %in% gender_input(),
  #     Equipment %in% equipment_input(),
  #     BodyweightKg <= bodyweight_input_max(),
  #     BodyweightKg >= bodyweight_input_min(),
  #     !is.na(BestBenchKg),
  #     !is.na(BestSquatKg),
  #     !is.na(BestDeadliftKg),
  #     !is.na(TotalKg)
  #     )
  # 
  #   # checks if there are data points in selected bodyweight range, if not shows error message.
  #   validate(
  #     need(nrow(data_untested )!=0 && nrow(data_tested )!=0, "Please select and appropriate range for bodyweights.")
  #   )
  # 
  #   col_index <- lift_kg_input() # which column is used (TotalKg, BestSquatKg, BestBenchKg, BestDeadliftKg)
  # 
  #   # creates distributions
  #   d1 <- distribution(data_untested[[col_index]])
  #   d2 <- distribution(data_tested[[col_index]])
  # 
  #   # creates plot
  #   plot(range(0, max(lifter_data$TotalKg[!is.na(lifter_data$TotalKg)])), range(d1$y, d2$y), type = "n", xlab = "Kilogram", ylab = "Density")
  #   lines(d1, col = "red")
  #   lines(d2, col = "blue")
  #   polygon(d1, col=rgb(1, 0, 0,0.5), border=NA) #fills the area under line
  #   polygon(d2, col=rgb(0, 0, 1,0.5), border=NA) #fills the area under line
  #   legend("topleft", legend=c("Untested", "Tested"),
  #          col=c("red", "blue"), lty=1)
  # })
  
#This is the logic for the parallel coordinate plot in Tab 2  
  output$parcoord <- renderPlotly({
    #untested data
    ud <- filter(
      untestedData,
      Equipment %in% equipment_input(), 
      BodyweightKg <= bodyweight_input_max(), 
      BodyweightKg >= bodyweight_input_min()
     )
    
    td <- filter(
      testedData,
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
      mean(um[,s1]), 
      mean(tm[,s1]), 
      mean(uf[,s1]),
      mean(tf[,s1])
      )
    bench <- c(
      mean(um[,s2]), 
      mean(tm[,s2]), 
      mean(uf[,s2]),
      mean(tf[,s2])
      )
    deadlift <- c(
      mean(um[,s3]),
      mean(tm[,s3]), 
      mean(uf[,s3]),
      mean(tf[,s3])
      )
    
    total <- c(
      mean(um[,s4]), 
      mean(tm[,s4]), 
      mean(uf[,s4]),
      mean(tf[,s4])
      )
    
    wilks <- c(
      mean(um[,s5]), 
      mean(tm[,s5]), 
      mean(uf[,s5]),
      mean(tf[,s5])
      )
    
    groups <- c(
      "Untested Male",
      "Tested Male", 
      "Untested Female", 
      "Tested Female"
      )
    
    data_all <- data.frame(Squat = squat,Bench = bench, Deadlift = deadlift,Total = total, Wilks = wilks, Groups = groups)

    
    #create the parallel coordinate plot
    ggparcoord(data_all, columns = 1:(ncol(data_all)-1), groupColumn = ncol(data_all), scale = "globalminmax", alphaLines = 1) + 
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
