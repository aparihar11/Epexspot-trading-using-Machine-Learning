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
library(tidyr)


###reading previous dataset
d2<-read.csv("C:/Users/aparihar/Documents/GitHub/electric_delware/datamart.csv") 
d2$fromdate<-as.Date(d2$fromdate,format="%d/%m/%Y")


###adding current dataset
d3<-read.csv("C:/Users/aparihar/Documents/GitHub/electric_delware/basetableApril14.csv") 
d2$datetime=ISOdatetime(year(d2$fromdate), month(d2$fromdate), day(d2$fromdate), d2$fromtime, 0, 0)
d3$datetime=ISOdatetime(year(d3$fromdate), month(d3$fromdate), day(d3$fromdate), d3$fromtime, 0, 0)

###merging data
myvars <- c("datetime", "ActualTotalLoad")
newdata <- d2[myvars]
d1<-merge(newdata,d3,by="datetime",all = TRUE)


###skewness between  forecast vs actual

d1$diffloadforecast<-as.integer(d1$Dayahead_Load.Forecast-d1$ActualTotalLoad)


options(shiny.maxRequestSize=30*1024^2)

#getwd()
#load("basetable.RData")

server <- function(input, output,session) {
    
#  newdata <- reactive({
#    filter(d2, between(datetime ,input$datetime[1], input$datetime[2]))
 # })
  
  
  output$table <- DT::renderDataTable({
    d1
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
  
  Temperature<-reactive({
    d4 <- d3 %>%
      select(datetime, TMIN, TMAX,TAVG) %>%
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





