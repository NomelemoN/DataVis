#Firstly install below libraries from the packages tab in lower left pane

library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(readr)

#=================Global Variables==================================================

## Meet Data
meet_data = read_csv("meets.csv")
meet_data$LatLong = paste(meet_data$lat,meet_data$long,sep = ":")

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
                                            value = max(meet_data$Year), animate = animationOptions(interval = 3000, loop = FALSE, playButton = TRUE, pauseButton = TRUE)),
                                
                                br(), ## Add a break line
                                testedFeds <- c('AAU','AsianPF','CommonwealthPF','CPU','EPF','FESUPO','FFForce','GBPF','IPF','IrishPF','NAPF','NASA','NIPF','NSF','NZPF','OceaniaPF','PA','RAW','THSPA','USAPL','WNPF'),
                                allFeds <- c('GPA', 'GPC', 'IPF', 'IPL', 'WPC','WUAP','AsianPF','CommonwealthPF','EPF','FESUPO','NAPF','OceaniaPF','365Strong','AAU','APA','APC','APF','HERC','IPA','MHP','NASA','RAW','RPS','RUPC','SPF', 'THSPA','UPA','USAPL','USPA','USPF','XPC','WNPF','CAPO','PA','ProRaw','CPF','CPL','CPU','FPO','FFFORCE','IrishPF','NZPF','NSF','BB','SCT','WRPF','GBPF','NIPF'),
                                checkboxGroupInput("feds", "Choose Federations:", allFeds,inline = TRUE),
                                checkboxInput('bar','All/None'),
                                br(), 
                                
                                fluidRow( 
                                  #row of content (full screen width is 12)
                                  
                                  box(width = 3, title = "Dummy box", 
                                      #dummy box which will fill 3/12 of row
                                      p("I have no purpose.")
                                  ),
                                  
                                  box(width = 9, title = "Number of meets per country",
                                      #another box that will fill 9/12 of row
                                      
                                      htmlOutput("intensity_map") 
                                      #We define contents ofthis htmlOutput in server function
                                      
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 12, title = "Number of meets per country",
                                      #another box that will fill 9/12 of row
                                      
                                      htmlOutput("intensity_map2") 
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
  
  
  
  ## Intensity map
  output$intensity_map <- renderGvis({
    #It says that intensity_map on the output is to be rendered as a 
    #google viusualization
    
    #Create data that needs to be displayed
    data <- filter(meet_data, Year==myYear()) %>% #Filters meets for the selected year 
      count(MeetCountry) #Counts the number of meet for each country 
    #and store it in a table called data with following format
    
    # MeetCountry       n
    # <chr>           <int>
    # 1   Australia      88
    # 2     Belarus       1
    # 3      Canada     108
    # 4     Czechia       1
    # 5     Denmark       2
    # 6     England       1
    # 7     Finland       5
    # 8      France       3
    # 9      Greece       1
    # 10     Ireland      9
    # 11      Israel      1
    # 12  Kazakhstan      2
    # 13    Malaysia      1
    # 14   N.Ireland      4
    # 15      Norway    100
    # 16      Poland     1
    # 17      Russia     6
    # 18       Spain     2
    # 19         USA    1096
    
    
    #Following chart is passed to the renderGvis funciton which generates the appropriate html for output
    gvisGeoChart(data, "MeetCountry", colorvar="n", 
                 ##location is specified by value "MeetCountry" in table "data"
                 ##color for a location is chosen by the associated value of valiable "n" in table "data"
                 options=list(colorAxis = "{colors: ['#e31b23']}",
                              width=800, height=500))
  })
  
  ## Intensity map
  output$intensity_map2 <- renderGvis({
    #It says that intensity_map on the output is to be rendered as a 
    #google viusualization
    
    #Create data that needs to be displayed
    data <- filter(meet_data, Year==myYear()) 
    
    
    #Following chart is passed to the renderGvis funciton which generates the appropriate html for output
    gvisGeoChart(data, "LatLong", colorvar = "NumberParticipants", hovervar = "MeetName",
                 options=list(colorAxis = "{colors: ['#e31b23']}",
                width=800, height=500, magnifyingGlass = "{enable: true, zoomFactor: 7.5}"))
  })
}

#==============Run the application (no change needed here)============================

shinyApp(ui = ui, server = server)

