shinyUI(navbarPage("National Transportation Statistics",
                     navbarMenu("Documentation",
                              
                     tabPanel("Table & Plot",
                              absolutePanel(
                                top = 30, left = 0, right = 0,
                                fixed = FALSE,
                                img(src="future_trans.jpg",width=2000,height=100),
                                tags$br(),tags$br(),
                                
                                mainPanel(
                                  img(src="doc_dataproducts_TP.png",width=800,height=500))
                                )
                              ),
                     tabPanel("Prediction",
                              absolutePanel(
                                top = 30, left = 0, right = 0,
                                fixed = FALSE,
                                img(src="future_trans.jpg",width=2000,height=100),
                                tags$br(),tags$br(),
                                
                                mainPanel(
                                  img(src="pred_dataproducts.png",width=800,height=500))
                                )
                              )
                     ),
                                
                              
                            
                     
                             
                    
                     tabPanel("Table",
                              sidebarPanel(
                                checkboxGroupInput(
                                  "mode_transportation","Mode of Transportation",
                                                   c("Air"="air|craft",
                                                     "Auto"="auto|car|vehicle|highway",
                                                     "Bus"="bus|motor bus",
                                                     "Rail"="rail|Amtrak")),
                                
                                selectInput(
                                  "select", label = ("Select a data set"), 
                                            choices = list("Passenger miles" = "passenger_miles", 
                                                           "Automobile cost" = "auto_cost", 
                                                           "Passenger fare" = "passenger_fare",
                                                           "Energy intensity"="energy_int")),
                                
                                sliderInput(
                                  "year", "Select year range:",min=1960, max=2014,
                                  step=1,sep=NULL,value=c(1960,2014),dragRange=TRUE
                                  )
                              ),
                              helpText(
                                textOutput("odata_set")
                                ),
                              mainPanel(
                                dataTableOutput('myTable')
                                )
                     ),
                     
                     
                     tabPanel("Plot",
                              sidebarPanel(
                                checkboxGroupInput(
                                  "mode_transportation2","Mode of Transportation",
                                                   c("Air"="air|craft",
                                                     "Auto"="auto|car|vehicle|highway",
                                                     "Bus"="bus|motor bus",
                                                     "Rail"="rail|Amtrak")
                                  ),
                                
                                selectInput(
                                  "select2", label = ("Select a data set"), 
                                            choices = list("Passenger miles" = "passenger_miles", 
                                                           "Automobile cost" = "auto_cost", 
                                                           "Passenger fare" = "passenger_fare",
                                                           "Energy intensity"="energy_int")
                                  ),
                                
                                sliderInput(
                                  "year2", "Select year range:", 
                                  min=1960, max=2014,step=1,sep=NULL,
                                  value=c(1960,2014),dragRange=TRUE
                                  )
                              ),
                              helpText(
                                textOutput("odata_set2")
                                ),
                              mainPanel(
                                plotOutput("myPlot"))
                     ),
                     tabPanel(
                       "Prediction",
                              sidebarPanel(
                                selectInput(
                                  "mode_transportation3",
                                          label= ("select a mode of transportation"),
                                          choices = c("Air"="air domestic",
                                                     "Auto"="^auto|^highway|15k$|^car",
                                                     "Bus"="^bus$",
                                                     "Rail"="^commuter|^Amtrak")
                                          ),
                                selectInput(
                                  "select3", label = ("Select a data set"), 
                                            choices = list("Passenger miles" = "passenger_miles", 
                                                           "Automobile cost" = "auto_cost", 
                                                           "Passenger fare" = "passenger_fare",
                                                           "Energy intensity"="energy_int")
                                  ),
                                selectInput(
                                  "year3", label = ("Select a year for prediction"),
                                            choices = c(2017:2030)
                                  ),
                                sliderInput(
                                  "year4", "Year range for linear fit model:", 
                                            min=1960, max=2030,step=1,sep=NULL,
                                            value=c(1960,2030),dragRange=TRUE
                                  )
                              ),
                              mainPanel(width=8,
                                h4('Results of prediction'),
                                verbatimTextOutput("myPrediction"),
                                h4("Linear fit through exisiting data"),
                                plotOutput("predPlot"))
                       )
                     )
          )
  
