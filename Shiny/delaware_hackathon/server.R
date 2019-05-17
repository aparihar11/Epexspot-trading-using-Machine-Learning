#install.packages("shinydashboard")
#install.packages("shinycssloaders")
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



d2<-read.csv("C:/Users/aparihar/Documents/GitHub/electric_delware/datamart.csv") 

d3<-read.csv("C:/Users/aparihar/Documents/GitHub/electric_delware/basetableApril14.csv") 
d2$datetime=ISOdatetime(year(d2$fromdate), month(d2$fromdate), day(d2$fromdate), d2$fromtime, 0, 0)
d3$datetime=ISOdatetime(year(d3$fromdate), month(d3$fromdate), day(d3$fromdate), d3$fromtime, 0, 0)
#d2 = subset(d2, select = -c(fromdate,fromtime,ï..) )
options(shiny.maxRequestSize=30*1024^2)

#getwd()
#load("basetable.RData")

server <- function(input, output,session) {
    
#  newdata <- reactive({
#    filter(d2, between(datetime ,input$datetime[1], input$datetime[2]))
 # })
  
  
  output$table <- DT::renderDataTable({
    d2
  })
  
  
  ###########Graph 1############
  output$graph1<- renderPlotly({
    output=test()
    output
  })
  
  ###########Graph 2############
  output$graph2<-renderPlotly({
    output=TopWage()
    output
    
  })
  
  ###########Graph 3############
  output$graph3<-renderPlotly({
    output=SuperStars()
    output
  })
  
  
  
  ###########Graph 4############
  output$graph4<-renderPlotly({
    new<-YoungestSquad()
    new
  })
  
  
  ###########Graph 7############
  output$graph5<-renderPlotly({
    new<-DayAhead()
    new
  })
  
  ###########Graph 8############
  output$graph6<-renderPlotly({
    new<-Intraday()
    new
  })
  
  ###########Graph 9############
  output$graph7<-renderDataTable({
    new<-MostUnfit()
    new
  })
  
  
  output$graph8<-renderPlotly({
    new<-VariationAge()
    new
  })
  
  output$graph9<-renderPlotly({
    new<-AgeWage()
    new
  })
  
  output$graph10<-renderDataTable({
    new<-CleanFinish()
    new
  })
  
  output$graph11<-renderDataTable({
    new<-VolleyGoal_85()
    new
  })
  
  
  output$graph12<-renderDataTable({
    new<-Penalties()
    new
  })
  
  
  
  ########### Day Ahead ##############
  DayAhead<-reactive({
    
    ggplot(d3, aes(datetime, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("") + ylab("Daily Views")
    
  })
  
  ########### Intraday Ahead Price ##############
  Intraday<-reactive({
    
    ggplot(d3, aes(datetime, IntradayPrice)) + geom_line() +
      xlab("") + ylab("Daily Views")
    
  })
  

  
  
} 





