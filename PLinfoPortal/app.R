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


#Types of federations
tested = c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF')
all = c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO', 'FFForce','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF')
untested = setdiff(all, tested)


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
                              
                                checkboxGroupInput("feds", "Choose Federations:", all, inline = TRUE),
                                checkboxGroupInput('type','Types', c('Tested', 'Untested'), inline = TRUE),
                                selectInput("countries", "Country", choices = union("All",meetCountries), label = "Countries",multiple = FALSE),
                                br(), 
                                
                                fluidRow( 
                                  #row of content (full screen width is 12)
                                  
                                  box(width = 12, title = "Meets across the world",
                                      #another box that will fill 9/12 of row
                                      
                                      leafletOutput("intensity_map") 
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
  ## Return selected federations
  myFeds <- reactive({
    if(!is.null(input$feds))
      input$feds
    else
      all
  })
  
  ## Return selected countries
  myCountry <- reactive({
    if(!is.null(input$countries) && !(input$countries == "All")){
      input$countries
    }
    else
      meetCountries
  })
  
  ## Return all feds of selected type
  type <-reactive({
    print(input$type)
    if(!is.null(input$type)){
      if("Tested" %in% input$type && "Untested" %in% input$type){
        print("c1")
        all
      }else if("Tested" %in% input$type){
        print("c2")
        tested
      }else if("Untested" %in% input$type){
        print("c3")
        untested
      }
    }else{
      print("c4")
      all
    }
  })
  
  output$intensity_map <- renderLeaflet({

    dataset <- filter(meet_data, Year==myYear(), Federation %in% myFeds(), Federation %in% type(), MeetCountry %in% myCountry()) %>% #Filters meets for the selected year
      count(MeetCountry) #Counts the number of meet for each country
    
    meets <- filter(meet_data, Year==myYear(), Federation %in% myFeds(), Federation %in% type(), MeetCountry %in% myCountry())

    
    world_spdf@data$n = 0
    if(nrow(dataset) != 0){
      for(row in 1:nrow(dataset)){
        index = match(dataset[[row, "MeetCountry"]], world_spdf@data$NAME)
        world_spdf@data$n[index] = dataset[[row, "n"]]
      }
    }
    
    #colors
    mybins=c(0,1,10,100,200,400, 800,Inf)
    mypalette = colorBin( palette="Oranges", domain=world_spdf@data$n, bins=mybins)
    
    #country title
    mytext=paste("Country: ", world_spdf@data$NAME,"<br/>", "Number of Meets: ", world_spdf@data$n, sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    if(length(meets[["MeetName"]]) != 0){
    leaflet(world_spdf) %>% 
      setView(lat = 53.0000, lng = 9.0000, zoom = 3) %>% #europe
      addTiles(options = tileOptions(noWrap = TRUE))  %>% 
      addPolygons( 
        fillColor = ~mypalette(n), stroke=TRUE, fillOpacity = 0.7, color="white", weight=0.4,
        highlight = highlightOptions( weight = 5, color = ~colorNumeric("Oranges", n)(n), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
        label = mytext,
        labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 7px"), textsize = "13px", direction = "auto")
      ) %>%
      addMarkers(data = meets, ~long, ~lat, popup = ~as.character(paste("<b> Number of participants: </b>", NumberParticipants, "<b> Date: </b>", Date, "<b> MeetCountry: </b>", MeetCountry, "<b> MeetState: </b>", MeetState, "<b> MeetTown: </b>", MeetTown, sep = "<br>")), label =~as.character(MeetName)) %>%
      addLegend( pal=mypalette, values=~n, opacity=0.9, title = paste("Number of Meets in",myYear(), sep = "<br>"), position = "bottomleft" )
    }else{
      leaflet(world_spdf) %>% 
        setView(lat = 53.0000, lng = 9.0000, zoom = 3) %>% #europe
        addTiles()  %>% 
        addPolygons( 
          fillColor = ~mypalette(n), stroke=TRUE, fillOpacity = 0.7, color="white", weight=0.4,
          highlight = highlightOptions( weight = 5, color = ~colorNumeric("Oranges", n)(n), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
          label = mytext,
          labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "2px 7px"), textsize = "13px", direction = "auto")
        ) %>%
        addLegend( pal=mypalette, values=~n, opacity=0.9, title = paste("Number of Meets in",myYear(), sep = "<br>"), position = "bottomleft" )
    }
      
  })
  

}

#==============Run the application (no change needed here)============================

shinyApp(ui = ui, server = server)

