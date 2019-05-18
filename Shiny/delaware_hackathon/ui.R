library(plotly)
library(shinydashboard)
title<-tags$a(href='https://www.delawareconsulting.com/en-us/',
              tags$img(src='img22.png', height='50', width='50'),
              'Trading Desk')




ui <- shinydashboard::dashboardPage(skin = "red",
                    dashboardHeader(title = title),
                    
                    dashboardSidebar(width = 350,
                                     tags$head(
                                       tags$style(HTML("
                                                       .sidebar { height: 90vh; overflow-y: auto; }
                                                       
                                                       " ))),
                                     
                                     sidebarMenu(
                                       menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                       menuItem("Menu", tabName = "Files", icon=icon("scale", lib = 'glyphicon'),
                                                menuItem("Intra Day Price", tabName = "da1", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Day Ahead Price", tabName = "da2", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Temperature Variation", tabName = "da3", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Forecast Skewness", tabName = "da4", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Weekly price", tabName = "da5", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Wind Speed Paris", tabName = "da6", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Wind Energy Solar Energy Temperature", tabName = "da7", icon=icon("triangle-right", lib = 'glyphicon')),
                                               
                                                menuItem("Wind Energy Production", tabName = "da8", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Production based on resource", tabName = "da9", icon=icon("triangle-right", lib = 'glyphicon')),
                                                dateRangeInput('date',
                                                               label = 'Date Range',
                                                               start = "2018-01-01", 
                                                               end = "2018-12-31",
                                                               max = "2018-12-31"
                                                )
                                                    
                                                
                                       )
                                       
                                     )),
                    
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName="data",
                                tabsetPanel(
                                  tabPanel("Data Files",
                                           wellPanel(div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                                           ))
                                  
                                )),
                        
                        # FirST tab content
                        
                        tabItem(tabName = "da1",
                                # FirST tab content
                                
                                tabPanel("Intraday Price",
                                         wellPanel(fluidRow(plotlyOutput("graph5")))
                                )
                        ),
                        
                        tabItem(tabName = "da2",
                                
                                tabPanel("Day Ahead Price",
                                         wellPanel(fluidRow(plotlyOutput("graph6")))
                                         
                                )
                        ),
                        tabItem(tabName = "da3",
                                tabPanel("Temperature Statistics",
                                         wellPanel(fluidRow(plotlyOutput("graph3")))
                                         
                                )
                        ),
                        
                        tabItem(tabName = "da4",
                                # FirST tab content
                                
                                tabPanel("Load Forecast Skewness",
                                         wellPanel(fluidRow(plotlyOutput("graph4")))
                                )
                        ),
                        
                        tabItem(tabName = "da6",
                                # FirST tab content
                                
                                tabPanel("WIND SPEED",
                                         wellPanel(fluidRow(plotlyOutput("graph1")))
                                )
                        )
                       
                        
                      ))
                    
                                       )
