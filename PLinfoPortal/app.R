#Firstly install below libraries from the packages tab in lower left pane

library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(countrycode)

#=================Global Variables==================================================

rm(list=ls())
## Meet Data
meet_data = read_csv("meets.csv")
meet_data$MeetCountry = gsub('England','United Kingdom',meet_data$MeetCountry)
meet_data$MeetCountry = gsub('N.Ireland','United Kingdom',meet_data$MeetCountry)

# Read the file with the rgdal library in R

world_spdf=readOGR( dsn= "TM_WORLD_BORDERS_SIMPL-0.3" , layer="TM_WORLD_BORDERS_SIMPL-0.3")

world_spdf@data$NAME = gsub('Saint Martin', 'Malta', world_spdf@data$NAME)
countrycodes = countrycode(world_spdf@data$NAME,'country.name','iso3c') #doesnt recognize all countries
meetCountries = unique(meet_data$MeetCountry)

for(row in 1:length(meetCountries)){ 
  print(meetCountries[row])
  index = match(countrycode(meetCountries[row],'country.name','iso3c'),countrycodes)
  print(world_spdf@data$NAME[index])
  world_spdf@data$NAME[index] = meetCountries[row]
  print(world_spdf@data$NAME[index])
}

# for(year in min(meet_data$Year):max(meet_data$Year)){ For Leaflet precomputation
#   dataset <- filter(meet_data, Year==year) %>% #Filters meets for the selected year
#     count(MeetCountry) #Counts the number of meet for each country
#   colName = 
#   world_spdf@data[, paste("Year", year, sep="")] = 0
#     if(nrow(dataset) != 0){
#       for(row in 1:nrow(dataset)){
#           print(year)
#           print(row)
#           
#           index = match(dataset[[row, "MeetCountry"]], world_spdf@data$NAME)
#           print(dataset[[row, "MeetCountry"]])
#           print(index)
#           world_spdf@data[index,paste("Year", year, sep="")] = dataset[row, "n"]
#           print(world_spdf@data[,paste("Year", year, sep="")])
#       }
#     }
# }

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
                                testedFeds <- c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF'),
                                allFeds <- c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF'),
                                checkboxGroupInput("feds", "Choose Federations:", allFeds,inline = TRUE),
                                checkboxGroupInput('bar','Types', c('Tested', 'Untested'), inline = TRUE),
                                br(), 
                                
                                fluidRow( 
                                  #row of content (full screen width is 12)
                                  
                                  box(width = 12, title = "Meets across the world",
                                      #another box that will fill 9/12 of row
                                      
                                      leafletOutput("intensity_map") 
                                      #We define contents ofthis htmlOutput in server function
                                      
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12, title = "Number of meets per country",
                                      #another box that will fill 9/12 of row
                                      
                                      leafletOutput("intensity_map2") 
                                      #We define contents ofthis htmlOutput in server function
                                      
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "drugs",
                                p("Qwerty")
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
  
  ## Return the year selected on the input slider
  myYear <- reactive({
    if(!is.null(input$year))
      input$year
    else
      2017
  })
  
  myFeds <- reactive({
    if(!is.null(input$feds))
      input$feds
    else
      c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF')
  })
  
  type <-reactive({
    if(!is.null(input$bar))
      if("Tested" %in% input$bar && "Untested" %in% input$bar){
        c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF')
      }
      if("Tested" %in% input$bar){
        c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF')
      }
      if("Untested" %in% input$bar){
        setdiff(c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF'),c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF'))
      }
    else
      c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF')
  })
  
  
  # # Intensity map
  # output$intensity_map <- renderGvis({
  #   #It says that intensity_map on the output is to be rendered as a
  #   #google viusualization
  # 
  #   #Create data that needs to be displayed
  #   data <- filter(meet_data, Year==myYear()) %>% #Filters meets for the selected year
  #     count(MeetCountry) #Counts the number of meet for each country
  #   #and store it in a table called data with following format
  # 
  #   # MeetCountry       n
  #   # <chr>           <int>
  #   # 1   Australia      88
  #   # 2     Belarus       1
  #   # 3      Canada     108
  #   # 4     Czechia       1
  #   # 5     Denmark       2
  #   # 6     England       1
  #   # 7     Finland       5
  #   # 8      France       3
  #   # 9      Greece       1
  #   # 10     Ireland      9
  #   # 11      Israel      1
  #   # 12  Kazakhstan      2
  #   # 13    Malaysia      1
  #   # 14   N.Ireland      4
  #   # 15      Norway    100
  #   # 16      Poland     1
  #   # 17      Russia     6
  #   # 18       Spain     2
  #   # 19         USA    1096
  # 
  # 
  #   #Following chart is passed to the renderGvis funciton which generates the appropriate html for output
  #   gvisGeoChart(data, "MeetCountry", colorvar="n",
  #                ##location is specified by value "MeetCountry" in table "data"
  #                ##color for a location is chosen by the associated value of valiable "n" in table "data"
  #                options=list(colorAxis = "{colors: ['#e31b23']}",
  #                             width=800, height=500))
  #   
  # })
  output$intensity_map <- renderLeaflet({
    #colName = paste("Year", myYear(), sep="") for precomputed version
    
    dataset <- filter(meet_data, Year==myYear(), Federation %in% myFeds() && Federation %in% type()) %>% #Filters meets for the selected year
      count(MeetCountry) #Counts the number of meet for each country
      world_spdf@data$n = 0
    if(nrow(dataset) != 0){
      for(row in 1:nrow(dataset)){
        index = match(dataset[[row, "MeetCountry"]], world_spdf@data$NAME)
        world_spdf@data$n[index] = dataset[[row, "n"]]
      }
    }
      
    # Create a color palette with handmade bins.
    mybins=c(0,1,10,100,200,400, 800,Inf)
    mypalette = colorBin( palette="Oranges", domain=world_spdf@data$n, bins=mybins)
    
    # Prepar the text for the tooltip:
    mytext=paste("Country: ", world_spdf@data$NAME,"<br/>", "Number of Meets: ", world_spdf@data$n, sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      addPolygons( 
        fillColor = ~mypalette(n), stroke=TRUE, fillOpacity = 0.7, color="white", weight=0.3,
        highlight = highlightOptions( weight = 5, color = ~colorNumeric("Oranges", n)(n), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
        label = mytext,
        labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=mypalette, values=~n, opacity=0.9, title = paste("Number of Meets in",myYear(), sep = " "), position = "bottomleft" )
  })
  
  output$intensity_map2 <- renderLeaflet({
    #Create data that needs to be displayed
    meets <- filter(meet_data, Year==myYear(),Federation %in% myFeds() && Federation %in% type())
    
    #Following chart is passed to the renderGvis funciton which generates the appropriate html for output
    leaflet(data = meets) %>% 
      addTiles() %>%
      addMarkers(~long, ~lat, popup = ~as.character(NumberParticipants), label =~as.character(MeetName) )
      # addPolygons(fillColor = ~pal(gdp08), 
      #             fillOpacity = 0.8, 
      #             color = "#BDBDC3", 
      #             weight = 1)
    
  })
}

#==============Run the application (no change needed here)============================

shinyApp(ui = ui, server = server)

