ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Delaware Electricity Price Prediction"),
                    
                    dashboardSidebar(width = 350,
                                     tags$head(
                                       tags$style(HTML("
                                                       .sidebar { height: 90vh; overflow-y: auto; }
                                                       
                                                       " ))),
                                     
                                     sidebarMenu(
                                       menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                       menuItem("Menu", tabName = "Files", icon=icon("scale", lib = 'glyphicon'),
                                                menuItem("Intra Day Price", tabName = "da5", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Day Ahead Price", tabName = "da6", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Weekly price", tabName = "da9", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Wind Forecast", tabName = "da1", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Wind Energy Solar Energy Temperature", tabName = "da2", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Temperature and electricity usage", tabName = "da3", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Wind Energy Production", tabName = "da4", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Production based on resource", tabName = "da4", icon=icon("triangle-right", lib = 'glyphicon')),
                                      dateRangeInput("daterange1", "Date range:",
                                                               start = "2001-01-01",
                                                               end   = "2010-12-31")                
                                                
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
                                tabPanel("Squad Value in Millions",
                                         wellPanel(fluidRow(plotlyOutput("graph1")))
                                         
                                )
                        ),
                        tabItem(tabName = "da2",
                                tabPanel("Top Wage",
                                         wellPanel(fluidRow(plotlyOutput("graph2")))
                                         
                                )
                        ),
                        tabItem(tabName = "da3",
                                tabPanel("Super Stars",
                                         wellPanel(fluidRow(plotlyOutput("graph3")))
                                         
                                )
                        ),
                        
                        tabItem(tabName = "da4",
                                # FirST tab content
                                
                                tabPanel("Youngest Squad",
                                         wellPanel(fluidRow(plotlyOutput("graph4")))
                                )
                        ),
                        tabItem(tabName = "da5",
                                # FirST tab content
                                
                                tabPanel("Intraday Price",
                                         wellPanel(fluidRow(plotlyOutput("graph5")))
                                )
                        ),
                        
                        tabItem(tabName = "da6",
                                
                                tabPanel("Day Ahead Price",
                                         wellPanel(fluidRow(plotlyOutput("graph6")))
                                         
                                )
                        ),
                        tabItem(tabName = "da7",
                                # FirST tab content
                                
                                tabPanel("Most Unfit",
                                         wellPanel(fluidRow(DT::dataTableOutput("graph7")))
                                )
                        ),
                        tabItem(tabName = "da8",
                                
                                tabPanel("Variation Age",
                                         wellPanel(fluidRow(plotlyOutput("graph8")))
                                         
                                )                
                        ),
                        tabItem(tabName = "da9",
                                
                                tabPanel("Age vs Wage",
                                         wellPanel(fluidRow(plotlyOutput("graph9")))
                                         
                                )                
                        )
                        # tabItem(tabName = "da10",
                        #         
                        #         tabPanel("Best Clean Finishers",
                        #                  wellPanel(fluidRow( DT::dataTableOutput("graph10")))
                        #                  
                        #         )
                        # ),
                        # tabItem(tabName = "da11",
                        #         
                        #         tabPanel("VolleyGoal_85",
                        #                  wellPanel(fluidRow( DT::dataTableOutput("graph11")))
                        #                  
                        #         )
                        # ),
                        # tabItem(tabName = "da12",
                        #         
                        #         tabPanel("Penalties",
                        #                  wellPanel(fluidRow( DT::dataTableOutput("graph12")))
                        #                  
                        #         )
                        # )
                        
                      ))
                    
                                       )
