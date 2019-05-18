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
setwd("C:/Users/aparihar/Documents/GitHub/electric_delware")
#d1<-read.csv("dataset.csv")

#d1$datetime<-as.POSIXct(d1$datetime)



#Intradaychart <- d1 %>% filter(datetime >= as.POSIXct("2018-01-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2018-12-31 01:00:00", tz="UTC"))
#dayaheadchart <- d1 %>% filter(datetime >= as.POSIXct("2018-01-01 01:00:00", tz="UTC") & datetime <= as.POSIXct("2018-12-31 01:00:00", tz="UTC"))


####WIND DATA AGGREGATION ############
finaltable<-read.csv("final_basetable.csv")

####CONVERT DATE FOR DAILY WEEKLY ANALYSIS ###########
finaldaily<-read.csv("final_basetable.csv")
finaldaily$fromdate<-as.Date(finaldaily$fromdate,format="%m/%d/%Y")
finaldaily<-finaldaily[,-2]





######AGGREGATE DAILY/WEEKLY ##################

finaldaily<-aggregate(. ~fromdate, data=finaldaily, mean, na.rm=TRUE)
finaldailydecreasing<-finaldaily[ order(finaldaily$fromdate, decreasing = TRUE ),]

options(shiny.maxRequestSize=30*1024^2)

#getwd()
#load("basetable.RData")

server <- function(input, output,session) {
    
#  newdata <- reactive({
#    filter(d2, between(datetime ,input$datetime[1], input$datetime[2]))
 # })
  
  subData <- reactive({
    finaldaily %>%
      filter(as.Date(fromdate) >= as.Date(input$date[1]),as.Date(fromdate) <= as.Date(input$date[2])
  )
  })


  
  output$table <- DT::renderDataTable({
    finaldailydecreasing[1:200,]
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
    ggplot( data=subData(), aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  
  ###########Graph 8############
  output$graph6<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
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
    
    ggplot( subData, aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  

  ########### Intraday Ahead Price ##############
  Intraday<-reactive({
###Manual range test   
     
    ggplot(subData, aes(fromdate, IntradayPrice)) + geom_line() +
      xlab("2018") + ylab("Intra Day Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  
  Temperature<-reactive({
    d4 <- finaldaily %>%
      select(datetime, TMIN, TMAX,TAVG)  %>%
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
    
    loaddiff <- finaldaily %>% 
      ggplot(loaddiff, aes(datetime, diffloadforecast)) + geom_line() +
      xlab("") + ylab("Skewness Load Forecast") +  geom_line(aes(color = "#00AFBB"), size = 1)
    
  })
  
  
  
} 





