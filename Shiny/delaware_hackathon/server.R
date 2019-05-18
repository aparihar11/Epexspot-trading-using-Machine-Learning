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
# setwd("C:/Users/aparihar/Documents/GitHub/electric_delware")
 datamartdaily<-read.csv("descriptive.csv")
 finaldaily<-read.csv("finaldaily.csv")
 datamartdaily$fromdate<-as.Date(datamartdaily$fromdate)
 finaldaily$fromdate<-as.Date(finaldaily$fromdate)

# ######DATAMART DESCRIPTIVE ANALYTICS########
# datamarthourly<-read.csv("datamart.csv")
# datamarthourly$fromdate<-as.Date(datamarthourly$fromdate,format="%d/%m/%Y")
# datamarthourly<-datamarthourly[,-c(1,3,22,23)]
# datamartdaily<-aggregate(. ~fromdate, data=datamarthourly, mean, na.rm=TRUE)
# datamartdaily$diffloadforecast<-as.integer(datamartdaily$Dayahead_Load.Forecast-datamartdaily$ActualTotalLoad)
# 
# 
# ####CONVERT DATE FOR DAILY WEEKLY ANALYSIS ###########
# finalhourly<-read.csv("final_basetable.csv")
# finalhourly$fromdate<-as.Date(finalhourly$fromdate,format="%m/%d/%Y")
# finalhourly<-finalhourly[,-2]
# finaldaily<-aggregate(. ~fromdate, data=finalhourly, mean, na.rm=TRUE)
# write.csv(datamartdaily, file="descriptive.csv")
# write.csv(finaldaily,file="finaldaily.csv")
######AGGREGATE DAILY/WEEKLY ##################




# d1<-d1[-which(d1$IntradayPrice %in% boxplot.stats(d1$IntradayPrice)$out), ]
# ###skewness between  forecast vs actual
# 




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
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  
  ###########Graph 6 DAYAHEAD       ############
  output$graph6<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  

 

} 





