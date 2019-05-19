# install.packages("shinydashboard")
# install.packages("shinycssloaders")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("plotly")
library(dplyr)
library(haven)
library(shiny)
library(ggplot2)
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggrepel)
library(Cairo)
library(data.table)
library(lubridate)
library(DT)
library(tidyr)


###SET ENVIROMENTAL VARIABLE
Sys.setenv(TZ='GMT')

# SET WORKING DIRECTORY
 #setwd("C:/Users/aparihar/Documents/GitHub/electric_delware")
 datamartdaily<-read.csv("descriptive.csv")
 finaldaily<-read.csv("finaldaily.csv")
 datamartdaily$fromdate<-as.Date(datamartdaily$fromdate)
 finaldaily$fromdate<-as.Date(finaldaily$fromdate)

 
 #WIND FORECASTED DATA
windforecast<-read.csv("windforecastdaily.csv")
windforecast$date<-as.Date(windforecast$date)






options(shiny.maxRequestSize=30*1024^2)


server <- function(input, output,session) {

  
  subData <- reactive({
    finaldaily %>%
      filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
  )
  })
  
  Tempdata <- reactive({
     finaldaily %>%
       select(fromdate, TMIN, TMAX,TAVG)  %>%
       gather(key = "variable", value = "value", -fromdate) %>%
       filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
       )
   })
   
   loadvariation <- reactive({
     datamartdaily %>% select(fromdate,diffloadforecast) %>%
       filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
       )
   })
   
   RelationWind <- reactive({
     finaldaily %>%
       select(fromdate,IntradayPrice,DayaheadPrice._EUR_MWh,windspeedKmph)  %>%
       gather(key = "variable", value = "value", -fromdate) %>%
       filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
       )
   })
   
   Intraddayvddayahead <- reactive({
     finaldaily %>%
       select(fromdate,IntradayPrice,DayaheadPrice._EUR_MWh)  %>%
       gather(key = "variable", value = "value", -fromdate) %>%
       filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
       )
   })
   
  
   
   
   
   ####OUTPUT DATATABLE  #########
   output$table <- DT::renderDataTable({
    tail(finaldaily,n=200)
  })
  

   ###########Graph 1 WIND SPEED1############
   output$graph1<- renderPlotly({
     ggplot(subData(), aes(fromdate, windspeedKmph)) + geom_line() +
       xlab("") + ylab("Wind Speed") +  geom_line(aes(color = "#00AFBB"), size = 1)
  
   })
  
  # ###########Graph 2  LOAD FORECAST############
   output$graph2<-renderPlotly({
  
       ggplot(loaddiff, aes(datetime, diffloadforecast)) + geom_line() +
       xlab("") + ylab("Skewness Load Forecast") +  geom_line(aes(color = "#00AFBB"), size = 1)
  
   })
  
  ###########Graph 3 TEMPERATURE DATA############
  output$graph3<-renderPlotly({
    ggplot(data=Tempdata(), aes(x = fromdate, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800","#00e70b")) +
      theme_minimal()  
  })
  
  
  
  ###########Graph 4  LOAD FORECAST 4############
  output$graph4<-renderPlotly({
    ggplot(loadvariation(), aes(fromdate, diffloadforecast)) + geom_line() +
      xlab("") + ylab("Skewness Load Forecast") +  geom_line(aes(color = "#00AFBB"), size = 1)
    
  })
  
  
  ###########Graph 5 INTRADAY      ############
  output$graph5<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, IntradayPrice)) + geom_line() + 
      xlab("2018") + ylab("IntraDayPrice") + scale_x_date(limits = c(input$date[1], input$date[2])) + geom_line(aes(color = "#00AFBB"), size = 1)
    
  })
  
  ###########Graph 6 DAYAHEAD       ############
  output$graph6<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() + 
      xlab("2018") + ylab("IntraDayPrice") + scale_x_date(limits = c(input$date[1], input$date[2])) + geom_line(aes(color = "#00AFBB"), size = 1)
  })
   
   ###########Graph 7 WIND VS PRICE       ############
   output$graph7<-renderPlotly({
     ggplot( data=RelationWind(), aes(fromdate, value)) +  geom_line(aes(color = variable), size = 1) +
       scale_color_manual(values = c("#00AFBB", "#E7B800","#00e70b")) +
       theme_minimal() + scale_x_date(limits = c(input$date[1], input$date[2]))
     
   })
   
   ###########Graph 8 WIND FORECAST       ############
   output$graph8<-renderPlotly({
     
     ggplot( data = windforecast, aes( date, speed )) + geom_line() 
   })
  
   ###########Graph 9 INTRA VS DAYAHEAD       ############
   output$graph9<-renderPlotly({
     ggplot( data=Intraddayvddayahead(), aes(fromdate, value)) +  geom_line(aes(color = variable), size = 1) +
       scale_color_manual(values = c("#00AFBB", "#E7B800")) +
       theme_minimal() + scale_x_date(limits = c(input$date[1], input$date[2]))
     
   })
   
 

} 





