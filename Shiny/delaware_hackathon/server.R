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

Sys.setenv(TZ='GMT')


# 
# d1<-d1[-which(d1$IntradayPrice %in% boxplot.stats(d1$IntradayPrice)$out), ]
# ###skewness between  forecast vs actual
# 
# d1$diffloadforecast<-as.integer(d1$Dayahead_Load.Forecast-d1$ActualTotalLoad)
#setwd("C:\\Users\\aparihar\\Documents\\GitHub\\electric_delware\\Shiny\\delaware_hackathon")
setwd("C:/Users/aparihar/Documents/GitHub/electric_delware")



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


  
  output$table <- DT::renderDataTable({
    tail(finaldaily,n=200)
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
    ggplot(data=Tempdata(), aes(x = fromdate, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800","#00e70b")) +
      theme_minimal()  
  })
  
  
  
  ###########Graph 4############
  output$graph4<-renderPlotly({
    new<-ActualvsforecastLoad()
    new
  })
  
  
  ###########Graph 5 INTRADAY############
  output$graph5<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, IntradayPrice)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  
  ###########Graph 6 DAYAHEAD############
  output$graph6<-renderPlotly({
    ggplot( data=subData(), aes(fromdate, DayaheadPrice._EUR_MWh)) + geom_line() +
      xlab("2018") + ylab("Day Ahead Price") + scale_x_date(limits = c(input$date[1], input$date[2]))
    
  })
  

  ActualvsforecastLoad<-reactive({
    
    loaddiff <- finaldaily %>% 
      ggplot(loaddiff, aes(datetime, diffloadforecast)) + geom_line() +
      xlab("") + ylab("Skewness Load Forecast") +  geom_line(aes(color = "#00AFBB"), size = 1)
    
  })

} 





