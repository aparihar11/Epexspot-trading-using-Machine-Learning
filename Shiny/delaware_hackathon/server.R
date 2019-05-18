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
#load("database.RData")
Sys.setenv(TZ='GMT')
###reading previous dataset
# d2<-read.csv("C:/Users/aparihar/Documents/GitHub/electric_delware/datamart.csv")
# d2$fromdate<-as.Date(d2$fromdate,format="%d/%m/%Y")
# 
# 
# ###adding current dataset

# ###merging data
# myvars <- c("datetime", "ActualTotalLoad")
# newdata <- d2[myvars]
# d1<-merge(newdata,d3,by="datetime",all = TRUE)
# 
# d1<-d1[-which(d1$IntradayPrice %in% boxplot.stats(d1$IntradayPrice)$out), ]
# ###skewness between  forecast vs actual
# 
# d1$diffloadforecast<-as.integer(d1$Dayahead_Load.Forecast-d1$ActualTotalLoad)
#setwd("C:\\Users\\aparihar\\Documents\\GitHub\\electric_delware\\Shiny\\delaware_hackathon")
d1<-read.csv("dataset.csv")
d1$datetime<-as.POSIXct(d1$datetime)

Intradaychart <- d1 %>% filter(datetime >= as.POSIXct("2018-01-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2018-12-31 01:00:00", tz="UTC"))
dayaheadchart <- d1 %>% filter(datetime >= as.POSIXct("2018-01-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2018-12-31 01:00:00", tz="UTC"))
options(shiny.maxRequestSize=30*1024^2)

#getwd()
#load("basetable.RData")

server <- function(input, output,session) {
    
#  newdata <- reactive({
#    filter(d2, between(datetime ,input$datetime[1], input$datetime[2]))
 # })
  
  
  output$table <- DT::renderDataTable({
    tail(d1,n=200)
  })
  
  
  ###########Graph 1############
  output$graph1<- renderPlotly({
    output=test()
    output
  })
  
  ###########Graph 2############
  output$graph2<-renderPlotly({
    output=ActualLoad()
    output
    
  })
  
  ###########Graph 3############
  output$graph3<-renderPlotly({
    output=Temperature()
    output
  })
  
  
  
  ###########Graph 4############
  output$graph4<-renderPlotly({
    new<-ActualvsforecastLoad()
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
    new<-na()
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
    
    ggplot(dayaheadchart, aes(datetime, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price")
    
  })
  
  ########### Intraday Ahead Price ##############
  Intraday<-reactive({
###Manual range test   
     
    ggplot(Intradaychart, aes(datetime, IntradayPrice)) + geom_line() +
      xlab("2018") + ylab("Intra Day Price")
    
  })
  
  Temperature<-reactive({
    d4 <- d1 %>%
      select(datetime, TMIN, TMAX,TAVG) %>% filter(datetime >= as.POSIXct("2018-01-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2018-12-31 01:00:00", tz="UTC")) %>%
      gather(key = "variable", value = "value", -datetime)
    head(d4, 3)
    ggplot(d4, aes(datetime, value)) + geom_line() +
      xlab("") + ylab("Temerature Variation")
    ggplot(d4, aes(x = datetime, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800","#00e70b")) +
      theme_minimal()
  })
  
  ActualvsforecastLoad<-reactive({
    
    loaddiff <- d1 %>% filter(datetime >= as.POSIXct("2016-07-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2016-08-01 01:00:00", tz="UTC")) 
      ggplot(loaddiff, aes(datetime, diffloadforecast)) + geom_line() +
      xlab("") + ylab("Skewness Load Forecast") +  geom_line(aes(color = "#00AFBB"), size = 1)
    
  })
  
  
  
} 





