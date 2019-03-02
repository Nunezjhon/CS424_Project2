#
# Name: Jhon Nunez & Daisy Arellano
# Class: CS424 Visualization and Visual Analytics 
# Spring 2019
#
# Project 2 Every Breath You Take
# Description: Visual Representation of Air Quality in counties througout the United States

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(gridExtra)
library(leaflet)
library(scales)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)

#------------------------------------------load all data files-----------------------------------------------

#import anual data from project 1
mydata1 = list.files(pattern="*.csv")
allData <- lapply(mydata1, read.csv )
mergedData <- do.call(rbind, allData)

#Creates subsets of my merged data files to display on sidebar
stateNames <- unique(mergedData$State)
countyNames <- subset(mergedData, select = c(State,County,Year))
years<-c(1990:2018)

#mergedDailyData$Date <- format(mergedDailyData$Date, format="%y-%m-%d")
#test <- mergedDailyData[mergedDailyData$Date < as.Date("1990-12-31"), ]
#test2 <- subset(test, select = c(Date, AQI))

#------------------------------------------------------------------------------------------------------------

#import daily data 
mydata2 <- list.files(path="daily", pattern = "*csv")
setwd("daily")
allData2 <- lapply(mydata2, read.csv )
mergedDailyData <- do.call(rbind, allData2)

#add fulldate column
mergedDailyData$FullDate <- format(mergedDailyData$Date, format="%y-%m-%d")

#format date column 
mergedDailyData$Date <- format(mergedDailyData$Date, format="%y-%m-%d")
#seperate date into 3 columns
mergedDailyData <- separate(mergedDailyData, col = "Date", into = c("Year","Month","Day"), sep = "-" )


#--------------------------------------------------Body-----------------------------------------------------------

#Dashboard page code which includes the header, sidebar, and body
ui <- dashboardPage(
  
  #Dashboard Title
  dashboardHeader(title = "Project 2"),
  
  #Dashboard Sidebar
  dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                   
                   
                   selectInput("Year", "Select the year to visualize", years, selected = 1990)
                   ,
                   selectInput("State", "Select the State to visualize", stateNames, selected = "Illinois")
                   ,
                   selectInput("County", "Select the County to visualize", countyNames, selected = "Cook"),
                   
                   
                   h3(" Author: Jhon Nunez & Daisy Arellano"),
                   h4("Project 2: Every Breath You Take"),
                   p(" Visualization of air quality data around the US"),
                   p(" Libraries Used:"),
                   p("ggplot2, lubridate, DT, grid, gridExtra, leaflet, scales, readr"),
                   p("Data:"),
                   p("Data imported from the United States Environmental Protection Agency")
                   
                   
  ),
  #Dashboard Body: contains all boxes that are shown on the dashboard
  dashboardBody(
    
    tabsetPanel(
      
      tabPanel("Yearly", 
    
          column(width = 3,
                 box(
                   width = 30, solidHeader = TRUE, status = "primary", plotOutput("Bar", height = 400)
                 ),
                 box(
                   title = "AQI Days", width = 15, solidHeader = TRUE, status = "primary",plotOutput("Pie", height = 350)
                 ),
                 box(
                   width = 30, solidHeader = TRUE, status = "primary",plotOutput("Table", height = 250)
                 )
          ), #end of column
          
          column(width = 4, 
                 fluidRow(
                   box(
                     width = 12, solidHeader = TRUE, status = "primary", plotOutput("space5", height = 300)
                   )
                 ),#end of fluidRow
                 
                 fluidRow(
                   box( title = "CO %", width = 4, solidHeader = TRUE, status = "primary", plotOutput("Pie1", height = 200)
                   ),
                   box(
                     title = "NO2 %", width = 4, solidHeader = TRUE, status = "primary", plotOutput("Pie2", height = 200)
                   ),
                   box(
                     title = "Ozone %", width = 4, solidHeader = TRUE, status = "primary",plotOutput("Pie3", height = 200)
                   )
                 ),#end of fluidRow
                 fluidRow(
                   box(
                     width = 12, solidHeader = TRUE, status = "primary", plotOutput("space7", height = 500)
                   )
                 )#end of fluidRow

          ),#end of column
          
          column(width = 4, 
                 fluidRow(
                   box(
                     width = 12, solidHeader = TRUE, status = "primary", plotOutput("space6", height = 300)
                   )
                 ), #end of fluidRow
                 fluidRow(
                   box(
                     title = "SO2 %", width = 4, solidHeader = TRUE, status = "primary", plotOutput("Pie4", height = 200)
                   ),
                   box(
                     title = "PM2.5 %", width = 4, solidHeader = TRUE, status = "primary", plotOutput("Pie5", height = 200)
                   ),
                   box(
                     title = "PM10 %", width = 4, solidHeader = TRUE, status = "primary", plotOutput("Pie6", height = 200)
                   )
                 ),#end of fluidRow
                 fluidRow(
                   box(
                     width = 12,   solidHeader = TRUE, status = "primary", plotOutput("space8", height = 300)
                   )
                 ),#end of fluidRow  
                  fluidRow(
                     box(
                       width = 12, solidHeader = TRUE, status = "primary", plotOutput("space9", height = 180)
                     )
                 )#end of fluidRow
          )#end of column
      ),#end of Yearly tabPanel
  
      #--------------------------------------------PROJECT 2---------------------------------------------------------
      
      tabPanel("Daily", 
               
               
               fluidRow( width = 12,
                   box(#line graph
                     title = "Daily AQI Data", width = 20,   solidHeader = TRUE, status = "success", plotlyOutput("daily1", height = 250)
                   )
               ),
               
               column(width = 6,  
                 fluidRow(
                   box(#Bar Chart project 2
                     width = 20, solidHeader = TRUE, status = "success", plotOutput("daily2", height = 300)
                   )
                 ),
                 
                 fluidRow(
                   box(#table project 2
                     width = 20, solidHeader = TRUE, status = "success", DT::dataTableOutput(outputId = "daily3", height = 200)
                   )
                 )
               )#end of column
               
              

               
      )#end of Daily tabPanel
    )#end of tabsetPanel
  )#end of body
)#end of dashboard page

#-------------------------------------------------Functions----------------------------------------------------------

#contains all function of the project
server <- function(input, output,session) {
  
  #color blind palette colors
  cbPalette <- reactive({(c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#F0E442")) })
  
  #Create minimal theme    
  empty_theme <- reactive({theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=15, face = "bold")
      ) 
  })
  
  #-----------------------------------------------Update Sidebar----------------------------------------------------
  
  #updates the county and state choces based on year input  
  observeEvent(input$Year,{
    updateSelectInput(session, 'County',
                      choices = countyNames$County[countyNames$Year == input$Year & 
                                                     countyNames$State == input$State])
  })
  #updates the county choices based on the states input
  observeEvent(input$State,{
    updateSelectInput(session, 'County',
                      choices = countyNames$County[countyNames$State == input$State & 
                                                     countyNames$Year == input$Year])
  })
  
  
  #--------------------------------create subset of data based on user input-----------------------------------------
  pollutants <- reactive({
    subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
           select = c(Good.Days, Moderate.Days,Unhealthy.for.Sensitive.Groups.Days,
                      Unhealthy.Days,Very.Unhealthy.Days,Hazardous.Days)) 
  })
  #extract numbers from the subset table
  pol <- reactive({ as.numeric(pollutants() ) })
  lbs <- reactive({ c("Good Days", "Moderate Days","Unhealthy for Sentitive Groups", "Unhealthy Days", 
                      "Very Unhealthy Days","Hazardous Days") })
  
  individualPollutants <- reactive({
    subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
           select = c(Days.CO, Days.NO2, Days.Ozone, Days.SO2, Days.PM2.5, Days.PM10) )
  })
  individualPol <- reactive({ as.numeric(individualPollutants() )  })
  indlbs <- reactive({ c("CO","NO2","Ozone","SO2","PM2.5","PM10") })
  
  #extract subset of mergedData
  COPollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                    select = c(Days.with.AQI,Days.CO) ) })
  
  
  #extract numbers needed from the subset table
  ZCOPol1 <- reactive({ as.numeric(COPollutants() )  })
  ZCOPol <- reactive({ 
    mod <- ZCOPol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  ZCOlbs <- reactive({ c("Days with AQI","ZCO") })
  
  
  #extract subset of mergedData
  NO2Pollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                     select = c(Days.with.AQI,Days.NO2) ) })
  #extract numbers needed from the subset table
  NO2Pol1 <- reactive({ as.numeric(NO2Pollutants() )  })
  NO2Pol <- reactive({ 
    mod <- NO2Pol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  NO2lbs <- reactive({ c("Days with AQI","NO2") })
  
  
  #extract subset of mergedData
  OzonePollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                       select = c(Days.with.AQI,Days.Ozone) ) })
  #extract numbers needed from the subset table
  OzonePol1 <- reactive({ as.numeric(OzonePollutants() )  })
  OzonePol <- reactive({ 
    mod <- OzonePol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  Ozonelbs <- reactive({ c("Days with AQI","Ozone") })
  
  
  #extract subset of mergedData
  SO2Pollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                     select = c(Days.with.AQI,Days.SO2) ) })
  #extract numbers needed from the subset table
  SO2Pol1 <- reactive({ as.numeric(SO2Pollutants() )  })
  SO2Pol <- reactive({ 
    mod <- SO2Pol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  SO2lbs <- reactive({ c("Days with AQI","SO2") })
  
  
  #extract subset of mergedData
  PM25Pollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                      select = c(Days.with.AQI,Days.PM2.5) ) })
  #extract numbers needed from the subset table
  PM25Pol1 <- reactive({ as.numeric(PM25Pollutants() )  })
  PM25Pol <- reactive({ 
    mod <- PM25Pol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  PM25lbs <- reactive({ c("Days with AQI","PM2.5") })
  
  
  #extract subset of mergedData
  PM10Pollutants <- reactive({ subset(mergedData,Year == input$Year & State == input$State & County == input$County, 
                                      select = c(Days.with.AQI,Days.PM10) ) })
  #extract numbers needed from the subset table
  PM10Pol1 <- reactive({ as.numeric(PM10Pollutants() )  })
  PM10Pol <- reactive({ 
    mod <- PM10Pol1()
    mod[1] <- (mod[1] - mod[2]) #substract second element from total
    mod
  })
  PM10lbs <- reactive({ c("Days with AQI","PM10") })
  
  #-----------------------------------------------------------------------------------------------------------------
  
  #Data (Median - Max AQI)
  allAQI <- reactive({ subset(mergedData, State == input$State & County == input$County, 
                              select = c(Year,Max.AQI,X90th.Percentile.AQI,Median.AQI)) })
  
  #Data (CO - PM10)
  allPollutants <- reactive({ 
    subset(mergedData, State == input$State & County == input$County, 
           select = c(Year,Days.with.AQI,Days.CO, Days.NO2, Days.Ozone, Days.SO2, Days.PM2.5, Days.PM10)) 
  })
  
  #Data for Pollutant table
  polTable1 <- reactive({
    subset(mergedData, Year == input$Year & State == input$State & County == input$County, 
           select = c(Days.CO, Days.NO2, Days.Ozone, Days.SO2, Days.PM2.5, Days.PM10))
  })
  
  polTable <- reactive({ as.numeric(polTable1() )  })
  total <- reactive({ sum(polTable()) })
  pollbs <- reactive({ c("CO","NO2","Ozone","SO2","PM2.5","PM10") })
  
  #-----------------------------------------------------NEW----------------------------------------------------------
  
  dailyAQI <- reactive({
    subset(mergedDailyData, Year == input$Year & county.Name == input$County, select = c(FullDate, AQI, Defining.Parameter))
  })
  
  dailyAQIFullDate <- reactive({  dailyAQI()$FullDate  })
  dailyAQIData <- reactive({  dailyAQI()$AQI  })
  dailyAQIPol <- reactive({  dailyAQI()$Defining.Parameter  })
  
  
  
  monthAQI <-  reactive({
    subset(mergedDailyData, Year == input$Year & county.Name == input$County , select = c(Year, Month, Category))
  })
  
  jan <- reactive({ subset(monthAQI(), Month == "01", select = c(Month,Category) ) })
  feb <- reactive({ subset(monthAQI(), Month == "02", select = c(Month,Category) ) })
  mar <- reactive({ subset(monthAQI(), Month == "03", select = c(Month,Category) ) })
  apr <- reactive({ subset(monthAQI(), Month == "04", select = c(Month,Category) ) })
  may <- reactive({ subset(monthAQI(), Month == "05", select = c(Month,Category) ) })
  jun <- reactive({ subset(monthAQI(), Month == "06", select = c(Month,Category) ) })
  jul <- reactive({ subset(monthAQI(), Month == "07", select = c(Month,Category) ) })
  aug <- reactive({ subset(monthAQI(), Month == "08", select = c(Month,Category) ) })
  sep <- reactive({ subset(monthAQI(), Month == "09", select = c(Month,Category) ) })
  oct <- reactive({ subset(monthAQI(), Month == "10", select = c(Month,Category) ) })
  nov <- reactive({ subset(monthAQI(), Month == "11", select = c(Month,Category) ) })
  dec <- reactive({ subset(monthAQI(), Month == "12", select = c(Month,Category) ) })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #-------------------------------------------Visualization Functions------------------------------------------------
  
  #outputs visual data to Bar
  output$Bar <- renderPlot({
    
    df <- data.frame(
      AQI = lbs(),
      days = pol()
    )
    bp <- ggplot(df, aes(x=AQI, y=days/sum(days),fill=AQI))+ geom_bar(width = 1, stat = "identity") + 
      scale_fill_brewer("AQI") + scale_y_continuous(name="Percentage",labels = scales::percent_format()) + 
      theme(axis.text.x = element_blank())
    bp
    
  })
  
  #outputs visual data to Pie
  output$Pie <- renderPlot({
    
    df <- data.frame(
      AQI = lbs(),
      days = pol()
    )
    bp <- ggplot(df, aes(x="", y=days,fill=AQI))+ geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() +scale_fill_brewer("AQI") + 
      theme(axis.text.x = element_blank() )+
      geom_text(aes(label = percent(days/sum(days))), position = position_stack(vjust = 0.5), size = 3)
    pie 
    
  })
  
  
  #outputs visual data to Table
  output$Table <- renderPlot({
    
    df <- data.frame(
      AQI = lbs(),
      Days = pol()
    )
    grid.table(df, rows = NULL)
  })
  
  #outputs visual data to space #5 
  output$space5 <- renderPlot({
    df <- data.frame(
      numbers = individualPol(), pollutants = indlbs()
    )
    bp <- ggplot(df, aes(x=pollutants, y=numbers/sum(numbers),fill = pollutants))+ 
      geom_bar(width = 1, stat = "identity") + scale_y_continuous(name="Percentage",labels = scales::percent_format())+
      scale_fill_manual(values = cbPalette())
    bp
  })
  
  #outputs visual data to space #6
  output$space6 <- renderPlot({
    df <- data.frame(
      Days = individualPol(), Pollutant = indlbs()
    )
    colnames(df)[colnames(df) == "Days"] <- "Days as the main pollutant"
    grid.table(df, rows = NULL)
    
  })
  
  #-------------------------------------------Percent Pie visualization----------------------------------------------
  #outputs visual data to Pie #1  
  output$Pie1 <- renderPlot({
    df <- data.frame( numbers = ZCOPol(), pollutants = ZCOlbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#E69F00") ) +
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    pie   
  })
  #outputs visual data to Pie #2
  output$Pie2 <- renderPlot({
    df <- data.frame( numbers = NO2Pol(), pollutants = NO2lbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#56B4E9") )+
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    pie
  })
  #outputs visual data to Pie #3
  output$Pie3 <- renderPlot({
    df <- data.frame( numbers = OzonePol(), pollutants = Ozonelbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#009E73") )+
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    
    pie 
  })
  #outputs visual data to Pie #4
  output$Pie4 <- renderPlot({
    df <- data.frame( numbers = SO2Pol(), pollutants = SO2lbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#F0E442") )+
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    pie
  })
  #outputs visual data to Pie #5
  output$Pie5 <- renderPlot({
    df <- data.frame( numbers = PM25Pol(), pollutants = PM25lbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#0072B2") )+
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    pie
  })
  #outputs visual data to Pie #6
  output$Pie6 <- renderPlot({
    df <- data.frame( numbers = PM10Pol(), pollutants = PM10lbs() )
    bp <- ggplot(df, aes(x="", y=numbers/sum(numbers),fill = pollutants))+geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y",start=0) + empty_theme() + theme(axis.text.x = element_blank() ) + 
      guides(fill = FALSE) + scale_fill_manual(values = c("#CCCCCC","#D55E00") )+
      geom_text(aes(label = percent(numbers/sum(numbers))), position = position_stack(vjust = 0.5), size = 3)
    pie
  })
  #------------------------------------------------------------------------------------------------------------------
  #outputs visual data to space #7
  output$space7 <- renderPlot({
    
    graph <- ggplot() + 
      geom_line(data = allAQI(), aes(x = Year, y = Max.AQI,colour = "Max"),size = 1) + 
      geom_line(data = allAQI(), aes(x = Year, y = X90th.Percentile.AQI,colour = "90th percentile"),size = 1) + 
      geom_line(data = allAQI(), aes(x = Year, y = Median.AQI,colour = "Median"), size = 1) + xlab("Year") + ylab("AQI") +
      scale_color_manual(name = "AQI", 
                         breaks = c("Max", "90th percentile","Median"),
                         values = c("#D55E00", "#CC79A7","#999999")
      )
    graph
  })
  #outputs visual data to space #8
  output$space8 <- renderPlot({
    graph <- ggplot() + geom_line(data = allPollutants(), aes(x = Year, y = Days.CO/Days.with.AQI ,colour = "CO"),size = 1)+ 
      geom_line(data = allPollutants(), aes(x = Year, y = Days.NO2/Days.with.AQI ,colour = "NO2"),size = 1)+
      geom_line(data = allPollutants(), aes(x = Year, y = Days.Ozone/Days.with.AQI, colour = "Ozone"),size = 1)+
      geom_line(data = allPollutants(), aes(x = Year, y = Days.SO2/Days.with.AQI ,colour = "SO2"),size = 1)+
      geom_line(data = allPollutants(), aes(x = Year, y = Days.PM2.5/Days.with.AQI ,colour = "PM2.5"),size = 1)+
      geom_line(data = allPollutants(), aes(x = Year, y = Days.PM10/Days.with.AQI ,colour = "PM10"),size = 1)+
      
      xlab("Year") + ylab("Pollutants") + 
      scale_color_manual(name = "Pollutants", 
                         breaks = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"), 
                         values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#F0E442")
      )+
      scale_y_continuous(labels = percent)
    
    graph
  })
  #outputs visual data to space #9
  output$space9 <- renderPlot({ 
    df <- data.frame(
      Pollutants = pollbs(),
      Percentage = (polTable()/total())*100
    )
    grid.table(df, rows = NULL)
  }) 
  
  
  
  
  #-----------------------------------------------------NEW----------------------------------------------------------
  
  
  output$daily1 <- renderPlotly({
    
    graph <- ggplot() + geom_line(data = dailyAQI(), aes(x = FullDate, y = AQI,colour = "AQI", group = 1)) + xlab("Date") + ylab("AQI") + 
      stat_smooth(color = "#FC4E07", fill="#FC4E07", method = "loess") 
    
    date <- dailyAQIFullDate()
    AQI <- dailyAQIData()
    pol <- dailyAQIPol()
    
    gg <- ggplotly(graph)
    gg <- style(gg, line = list(color = 'green'), hoverinfo = "text", text = paste("Date:", date, 
                                                                                       "<br>", 
                                                                                       "AQI:", AQI, 
                                                                                       "<br>",
                                                                                       "Highest pollutant:", pol 
                                                                                   ) )
    })
  
    output$daily2 <- renderPlot({
        
      m1 <- jan()
      df <- data.frame(January = "January", m1[2])
      m2 <- feb()
      df2 <- data.frame(February = "February", m2[2])
      m3 <- mar()
      df3 <- data.frame(March = "March", m3[2])
      m4 <- apr()  
      df4 <- data.frame(April = "April", m4[2])
      m5 <- may()
      df5 <- data.frame(May = "May", m5[2])
      m6 <- jun()
      df6 <- data.frame(June = "June", m6[2])
      m7 <- jul()
      df7 <- data.frame(July = "July", m7[2])
      m8 <- aug()
      df8 <- data.frame(August = "August", m8[2])
      m9 <- sep()
      df9 <- data.frame(September = "September", m9[2])
      m10 <- oct()
      df10 <- data.frame(October = "October", m10[2])
      m11 <- nov()
      df11 <- data.frame(November = "November", m11[2])
      m12 <- dec()
      df12 <- data.frame(December = "December", m12[2])
                        
      plot <- ggplot() + 
        geom_bar(aes(y = Category, x = January, fill = Category), data = df, stat="identity")+
        geom_bar(aes(y = Category, x = February, fill = Category), data = df2, stat="identity")+
        geom_bar(aes(y = Category, x = March, fill = Category), data = df3, stat="identity")+
        geom_bar(aes(y = Category, x = April, fill = Category), data = df4, stat="identity")+
        geom_bar(aes(y = Category, x = May, fill = Category), data = df5, stat="identity")+
        geom_bar(aes(y = Category, x = June, fill = Category), data = df6, stat="identity")+
        geom_bar(aes(y = Category, x = July, fill = Category), data = df7, stat="identity")+
        geom_bar(aes(y = Category, x = August, fill = Category), data = df8, stat="identity")+
        geom_bar(aes(y = Category, x = September, fill = Category), data = df9, stat="identity")+
        geom_bar(aes(y = Category, x = October, fill = Category), data = df10, stat="identity")+
        geom_bar(aes(y = Category, x = November, fill = Category), data = df11, stat="identity")+
        geom_bar(aes(y = Category, x = December, fill = Category), data = df12, stat="identity")
      
      plot
        
        
    })
    
    
    output$daily3 <- DT::renderDataTable(
      DT::datatable({       
        df <- as.data.frame( monthAQI() ) 
                   }, options = list(searching = FALSE, lengthChange = FALSE, pageLength = 3) 
      )
    )
    
    
    
    
  
  
  
  
  
}# end
# Run the application
shinyApp(ui, server)

