# import libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)
library(scales)
library(jpeg)
library(grid)
library(leaflet)

# get all of the tsv files in the same directory
data_daily <- lapply("CTA_daily.tsv", read.delim)
CTA_daily <- do.call(rbind, data_daily)

data_location <- lapply("CTA_Location.tsv", read.delim)
CTA_location <- do.call(rbind, data_location)

# format the date
CTA_daily <- CTA_daily[complete.cases(CTA_daily), ]
CTA_daily$date <- ymd(CTA_daily$date)

# convert the rides from int to numbers
CTA_daily$rides <- as.numeric(CTA_daily$rides)

CTA_daily_reverse <- CTA_daily
CTA_daily_reverse$stationname <- factor(CTA_daily_reverse$stationname, levels = rev(unique(CTA_daily_reverse$stationname)))

# Create the menu items to select the different years and the different stations
years<-c(2001:2021)
views<- c("All Station", "Year", "Month", "Date","Day")
mapViews <- c('Default','Contrast','Geographic')
station_names <- sort(unique(CTA_daily$stationname))

# Create the shiny dashboard
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "CTA Daily Entries"),
  dashboardSidebar(disable = TRUE, collapsed = TRUE,
                   
                   sidebarMenu( id = 'tabs',
                     menuItem("About", tabName = "about", icon = NULL),
                     menuItem("Analytic Dashboard", tabName = "dashboard", icon = NULL, selected = TRUE)
                     )
                  
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          column(1,
                 div(style ='height: 1300px; display: flex; flex-direction: column;align-items: center; justify-content: center;',
                     box(width =12,
                         tabsetPanel(type = "tabs",
                         tabPanel("Upper", box(width =12,fluidRow(
                           selectInput("viewPanel1", "View", views, selected = "All Station"),
                           selectInput("mapTile1", "Map View", mapViews, selected = "Default"),
                           conditionalPanel(condition = "input.viewPanel1 != 'All Station'",div(
                             selectInput("StationName1", "Station", station_names, selected = "UIC-Halsted"),
                             sliderInput("Year1","Year",value = 2021, min = 2001,max = 2021,sep="")
                           )),
                           conditionalPanel(condition = "input.viewPanel1 == 'All Station'",div(
                            checkboxInput('reorder1', 'Order min-max', value = FALSE),
                            dateInput("date1", label = h3("Date"), value = "2021-08-23", min = min(CTA_daily$date), max=max(CTA_daily$date)),
                             fluidRow(
                               column(6,actionButton('prevDate1', '<< previous day')),
                               column(6,actionButton('nextDate1', 'next day >>')),
                             )
                             ))
                         ))),
                         tabPanel("Lower", box(width =12,fluidRow(
                           selectInput("viewPanel2", "View", views, selected = "All Station"),
                           selectInput("mapTile2", "Map View", mapViews, selected = "Default"),
                           conditionalPanel(condition = "input.viewPanel2 != 'All Station'",div(
                             selectInput("StationName2", "Station", station_names, selected = "O'Hare Airport"),
                             sliderInput("Year2", "Year",value = 2021,min = 2001,max = 2021,sep="")
                           )),
                           conditionalPanel(condition = "input.viewPanel2 == 'All Station'",div(
                             checkboxInput('reorder2', 'Ordermin-max', value = FALSE),
                             dateInput("date2", label = h3("Date"), value = "2021-08-23", min = min(CTA_daily$date), max=max(CTA_daily$date)),
                             fluidRow(
                               column(6,actionButton('prevDate2', '<< previous day')),
                               column(6,actionButton('nextDate2', 'next day >>')),
                             )
                           ))
                         )))
                     ))
                 ),
                 div(style='display: flex;justify-content: center;align-items: flex-start; margin-bottom: 20px',
                     actionButton('aboutTab', 'about')) ,style='background: whitesmoke;'),
          column(11,
                 fluidRow(
                 
                   fluidRow(style='width: 100%;',
                      column( 12,
                              fluidRow(
                        
                        
                      column(12,h2(textOutput("Tab1"))),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel1 == 'All Station'",
                                              div(
                                                h3("All Stations"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafStations1", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histStations1", height = 500)
                                                           )
                                                         )
                                                  ),
                                                
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabStations1", height = 500)
                                                           )
                                                           
                                                         )),
                                              ))
                             )),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel1 == 'Year'",
                                              div(
                                                h3("All Year"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafYear1", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histYear1", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabYear1", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                        column(12,
                               conditionalPanel(condition = "input.viewPanel1 == 'Month'",
                                                div(
                                                  h3("Each Month"),
                                                  fluidRow(
                                                    column(2,
                                                           fluidRow(
                                                             
                                                             box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                                 leafletOutput("leafMonth1", height = 500)
                                                             )
                                                           )),
                                                    column(8,
                                                           fluidRow(
                                                             box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                  plotOutput("histMonth1", height = 500)
                                                             )
                                                           )
                                                    ),
                                                    
                                                    column(2,
                                                           fluidRow(
                                                             box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                                 dataTableOutput("tabMonth1", height = 500)
                                                             )
                                                             
                                                           )),
                                                  ))
                               )),
                        column(12,
                               conditionalPanel(condition = "input.viewPanel1 == 'Date'",
                                                div(
                                                  h3("Each Date"),
                                                  fluidRow(
                                                    column(2,
                                                           fluidRow(
                                                             
                                                             box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                                 leafletOutput("leafDate1", height = 500)
                                                             )
                                                           )),
                                                    column(8,
                                                           fluidRow(
                                                             box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                  plotOutput("histDate1", height = 500)
                                                             )
                                                           )
                                                    ),
                                                    
                                                    column(2,
                                                           fluidRow(
                                                             box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                                 dataTableOutput("tabDate1", height = 500)
                                                             )
                                                             
                                                           )),
                                                  ))
                               )),
                        column(12,
                               conditionalPanel(condition = "input.viewPanel1 == 'Day'",
                                                div(
                                                  h3("Each Day"),
                                                  fluidRow(
                                                    column(2,
                                                           fluidRow(
                                                             
                                                             box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                                 leafletOutput("leafDay1", height = 500)
                                                             )
                                                           )),
                                                    column(8,
                                                           fluidRow(
                                                             box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                  plotOutput("histDay1", height = 500)
                                                             )
                                                           )
                                                    ),
                                                    
                                                    column(2,
                                                           fluidRow(
                                                             box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                                 dataTableOutput("tabDay1", height = 500)
                                                             )
                                                             
                                                           )),
                                                  ))
                               ))
                          
            ))),
          
          fluidRow(style='width: 100%;background-color: #DCDCDC;',
            column( 12,
                    fluidRow(
                      
                      
                      column(12,h2(textOutput("Tab2"))),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel2 == 'All Station'",
                                              div(
                                                h3("All Stations"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafStations2", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histStations2", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabStations2", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel2 == 'Year'",
                                              div(
                                                h3("All Year"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafYear2", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histYear2", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabYear2", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel2 == 'Month'",
                                              div(
                                                h3("Each Month"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafMonth2", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histMonth2", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabMonth2", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel2 == 'Date'",
                                              div(
                                                h3("Each Date"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafDate2", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histDate2", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabDate2", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                      column(12,
                             conditionalPanel(condition = "input.viewPanel2 == 'Day'",
                                              div(
                                                h3("Each Day"),
                                                fluidRow(
                                                  column(2,
                                                         fluidRow(
                                                           
                                                           box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                               leafletOutput("leafDay2", height = 500)
                                                           )
                                                         )),
                                                  column(8,
                                                         fluidRow(
                                                           box( title = "Entries For Each Stations", solidHeader = TRUE, status = "primary", width = 12,
                                                                plotOutput("histDay2", height = 500)
                                                           )
                                                         )
                                                  ),
                                                  
                                                  column(2,
                                                         fluidRow(
                                                           box(title = "Entries For Each Stations as Table", solidHeader = TRUE, status = "primary", width = 12,
                                                               dataTableOutput("tabDay2", height = 500)
                                                           )
                                                           
                                                         )),
                                                ))
                             )),
                    )))
        
      )))),
      tabItem(tabName = "about",
              fluidRow(
                h1("About"),
               div(
                 span("Written by Athalia Rochelle Handowo for CS 424 Project 2 Spring 2022 on March Data taken from Chicago Data Portal on March 4, 2022 ",
                  style = "white-space: pre-wrap"),
                 a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f", "(link)"),
                 span(". The application display the number of rides/entries of the chosen stations per chosen timeframe and year with its map location."),
                 style='display:block;font-size:20px;'),
              ),
              div(actionButton('dashboardTab', 'dashboard'), style= 'display: flex;align-items: flex-end;height: 1000px;')
              ,style='padding-right: 15px;padding-left: 15px;')
    )
  ))

server <- function(input, output,session) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  output$Tab1 <- renderText({ input$StationName1 })
  output$Tab2 <- renderText({ input$StationName2 })
  
  observeEvent(input$aboutTab, {
    newtab <- switch(input$tabs, "dashboard" = "about","about" = "dashboard")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$dashboardTab, {
    newtab <- switch(input$tabs, "about" = "dashboard","dashboard" = "about")
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$nextDate1, {
    updateDateInput(session, "date1",value = ymd(input$date1) + days(1))
  })
  
  observeEvent(input$nextDate2, {
    updateDateInput(session, "date2",value = ymd(input$date2) + days(1))
  })
  
  observeEvent(input$prevDate1, {
    updateDateInput(session, "date2",value = ymd(input$date1) - days(1))
  })
  
  observeEvent(input$prevDate2, {
    updateDateInput(session, "date2",value = ymd(input$date2) - days(1))
  })
  

  
  # generate data for window 1
  allStationDateReactive1 <- reactive({subset(CTA_daily, date == input$date1)})
  
  stationNameReactive1 <- reactive({subset(CTA_daily, stationname == input$StationName1)})
  stationNameandYearsReactive1 <- reactive({subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)})

  stationNameLocationReactive1 <- reactive({subset(CTA_location, station_id == CTA_daily$station_id[[which(CTA_daily$stationname== input$StationName1)[1]]])})
  
  
  stationNameYearsReactive1 <- reactive({
    data <- subset(CTA_daily, stationname == input$StationName1)
    tapply(data$rides, year(data$date), FUN=sum)
  })
  stationNameMonthsReactive1 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
    tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
    })
  stationNameDaysReactive1 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
    tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  })
  
  # generate data for window 2
  allStationDateReactive2 <- reactive({subset(CTA_daily, date == input$date2)})
  
  stationNameReactive2 <- reactive({subset(CTA_daily, stationname == input$StationName2)})
  stationNameandYearsReactive2 <- reactive({subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)})
  
  stationNameLocationReactive2 <- reactive({subset(CTA_location, station_id == CTA_daily$station_id[[which(CTA_daily$stationname== input$StationName2)[1]]])})
  
  stationNameYearsReactive2 <- reactive({
    data <- subset(CTA_daily, stationname == input$StationName2)
    tapply(data$rides, year(data$date), FUN=sum)
  })
  stationNameMonthsReactive2 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
    tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
  })
  stationNameDaysReactive2 <- reactive({
    data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
    tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  })
  
  # 
  # window 1
  # 
  
  # show a bar chart of entries on date1 at all Station
  output$histStations1 <- renderPlot({
    allStationDate <- allStationDateReactive1()
    if ( input$reorder1){
      ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides)) + 
        labs(x="Station Name", y = "Rides") + 
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 45))
    }
    else{
      ggplot(allStationDate, aes(x=stationname, y=rides)) + 
        labs(x="Station Name", y = "Rides") + 
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 45))
    }
  })
  
  # show a bar chart of entries per Year at StationName1
  output$histYear1 <- renderPlot({
    oneYear <- stationNameReactive1()
    
    ggplot(oneYear, aes(x=year(date), y=rides)) + 
      labs(x="Years", y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  # show a bar chart of entries per Date at StationName1
  output$histDate1 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=date, y=rides)) + 
      labs(x=paste("Dates in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of entries per Month at StationName1
  output$histMonth1 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
      labs(x=paste("Months in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })

  
  # show a bar chart of entries per Day at StationName1
  output$histDay1 <- renderPlot({
    oneYear <- stationNameandYearsReactive1()

    ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
      labs(x=paste("Days in", input$Year1), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a table of entries on date1 on all stations
  output$tabStations1 <- DT::renderDataTable(
    if ( input$reorder1){
      DT::datatable({
        allStationDate <- allStationDateReactive1()
        rides <-  allStationDate$rides
        Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asc'))
      ), rownames = FALSE
      )
    }
    else{
      DT::datatable({
      allStationDate <- allStationDateReactive1()
      rides <-  allStationDate$rides
      Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
    },
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
    }
  )

  # show a table of entries per Year at StationName1
  output$tabYear1 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameYearsReactive1()
      Dates <- data.frame(years = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  # show a table of entries per Date at StationName1
  output$tabDate1 <- DT::renderDataTable(
    DT::datatable({
      oneYear <-  stationNameandYearsReactive1()
      Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  # show a table of entries per Month at StationName1
  output$tabMonth1 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameMonthsReactive1()
      Months <- data.frame(months = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  # show a table of entries per Day at StationName1  
  output$tabDay1 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameDaysReactive1()
      Dates <- data.frame(days=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  # 
  # window 2
  # 
  
  # show a bar chart of entries on date2 at all Station
  output$histStations2 <- renderPlot({
    allStationDate <- allStationDateReactive2()
    
    if ( input$reorder2){
      ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides)) + 
        labs(x="Station Name", y = "Rides") + 
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 45))
    }
    else{
      ggplot(allStationDate, aes(x=stationname, y=rides)) + 
        labs(x="Station Name", y = "Rides") + 
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 45))
    }
  })
  
  # show a bar chart of entries per Year at StationName2
  output$histYear2 <- renderPlot({
    oneYear <- stationNameReactive2()
    
    ggplot(oneYear, aes(x=year(date), y=rides)) + 
      labs(x='Years', y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of entries per Date at StationName2
  output$histDate2 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=date, y=rides)) + 
      labs(x=paste("Dates in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of entries per Month at StationName2
  output$histMonth2 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
      labs(x=paste("Months in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  
  # show a bar chart of entries per Day at StationName2
  output$histDay2 <- renderPlot({
    oneYear <- stationNameandYearsReactive2()
    
    ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
      labs(x=paste("Days in", input$Year2), y = "Rides") + 
      geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  })
  
  # show a table of entries on date1 on all stations
  output$tabStations2 <- DT::renderDataTable(
    if ( input$reorder2){
      DT::datatable({
        allStationDate <- allStationDateReactive2()
        rides <-  allStationDate$rides
        Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'asc'))
      ), rownames = FALSE
      )
    }
    else{
      DT::datatable({
        allStationDate <- allStationDateReactive1()
        rides <-  allStationDate$rides
        Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(0, 'asc'))
      ), rownames = FALSE
      )
    }
  )
  
  # show a table of entries per Year at StationName2
  output$tabYear2 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameYearsReactive2()
      Dates <- data.frame(years=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  # show a table of entries per Date at StationName2
  output$tabDate2 <- DT::renderDataTable(
    DT::datatable({
      oneYear <-  stationNameandYearsReactive2()
      Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  # show a table of entries per Month at StationName2
  output$tabMonth2 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameMonthsReactive2()
      Months <- data.frame(months = names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  # show a table entries per Day at StationName2
  output$tabDay2 <- DT::renderDataTable(
    DT::datatable({
      rides <-  stationNameDaysReactive2()
      Dates <- data.frame(days=names(rides), rides=rides)
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
  output$leafStations1 <- renderLeaflet({
    location <- stationNameLocationReactive1()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile1 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafYear1<- renderLeaflet({
    location <- stationNameLocationReactive1()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
    
    if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile1 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafMonth1<- renderLeaflet({
    location <- stationNameLocationReactive1()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
    
    if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile1 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafDay1<- renderLeaflet({
    location <- stationNameLocationReactive1()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
    
    if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile1 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafDate1<- renderLeaflet({
    location <- stationNameLocationReactive1()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
    
    if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile1 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  
  output$leafStations2 <- renderLeaflet({
    location <- stationNameLocationReactive2()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile2 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafYear2<- renderLeaflet({
    location <- stationNameLocationReactive2()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
    
    if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile2 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafMonth2<- renderLeaflet({
    location <- stationNameLocationReactive2()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
    
    if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile2 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafDay2<- renderLeaflet({
    location <- stationNameLocationReactive2()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
    
    if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile2 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  output$leafDate2<- renderLeaflet({
    location <- stationNameLocationReactive2()
    map <- leaflet(CTA_location)
    map <- addTiles(map)
    longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
    latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
    
    if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
    if(input$mapTile2 == 'Geographic') {
      map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
      map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
    }
    
    map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
    map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
    map
  })
  
  
}

shinyApp(ui = ui, server = server)

