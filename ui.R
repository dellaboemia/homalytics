# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

#ui.R

require(rCharts)
options(RCHART_LIB = 'polycharts')
library(shiny)
library(shinydashboard)

dashboardPage(skin = "green",
              dashboardHeader(title = "HomAlytics"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Market Explorer", tabName = "explorer", icon = icon("search")),
                  menuItem("Value Analysis", tabName = "valueAnalysis", icon = icon("area-chart")),
                  menuItem("Forecast Models", icon = icon("line-chart"),
                           menuSubItem("Model Training", icon = icon("gears"),tabName = "trainModels"),
                           menuSubItem("Model Comparison", icon = icon("check-circle"), tabName = "compareModels")),
                  menuItem("Market Forecast", tabName = "forecast", icon = icon("bar-chart"))
                )
              ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidPage(
                            title = "Dashboard",
                            fluidRow(
                              valueBoxOutput("hviUSBox"),
                              valueBoxOutput("MonthlyUSBox"),
                              valueBoxOutput("AnnualUSBox")
                            ),
                            fluidRow(
                              column(width = 4,
                                box(
                                  title = "Analytics for the Real Estate Market",
                                  status = "warning",
                                  width = 12,
                                  solidHeader = TRUE,
                                  h3("Welcome to HomAlytics"),
                                  p(class = "text-muted",
                                    paste("Here, we use statistical inference and forecast modeling techniques to 
                                    explore and forecast over 13,000 real estate markets in the United States.  
                                    This tool will enable you to:")),
                                  tags$ul(
                                    class = "text-muted",
                                    tags$li("explore home price indices and growth rates across various markets at a granular level"),
                                    tags$li("graphically engage home price data over time "),
                                    tags$li("decompose price movements into their seasonal, trend and irregular components"),
                                    tags$li("train the most popular forecasting models and compare their predictive accuracy"),
                                    tags$li("use these models to forecast home prices in virtually every US real estate market")
                                  ),
                                  p(class = "text-muted",
                                    paste("The menus to the left will walk you through the process of exploring markets, reviewing price trends, training 
                                    forecast models, evaluating model performance accuracy and predict home prices, 3, 5 or 10 years out.")),
                                  p(class = "text-muted",
                                    paste("To get started, click on the Market Explorer menu on the left."))
                                  )
                                ),
                              column(width = 8,
                                box(
                                  title = "Top 10 States by Annual Home Value Growth",
                                  status = "warning",
                                  width = 12,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  showOutput("top10States", "polycharts")
                                ),
                                box(
                                  title = "Top 10 Counties by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  showOutput("top10Counties", "polycharts")
                                ),
                                box(
                                  title = "Top 10 Cities by Annual Home Value Growth",
                                  status = "success",
                                  width = 12,
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  showOutput("top10Cities", "polycharts")
                                ) #End of Box
                              ) # End of column
                          ) # End of Fluid Row
                      ) # End of FluidPage
                  ), # End of tabItem
                  tabItem(tabName = "explorer",
                    fluidPage(
                      title = "Market Explorer",
                      fluidRow(
                          box(
                            title = "Query Builder",
                            status = "warning",
                            width = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            column(width = 4,
                              box(
                                title = "Home Value Range",
                                status = "primary",
                                solidHeader = FALSE,
                                width = 12,
                                p(class = "text-muted",
                                  paste("Select minimum and maximum home values")),
                                sliderInput("hviQuery", label = "Home Value Range", min = 0, max = 2000000, value = c(300000,500000)),
                                checkboxInput("maxValue", label = "Include values exceeding $2m", value = FALSE)
                              ), # end of box
                              actionButton("query", label = "Go")
                            ), # end of column
                            column(width = 4,
                              box(
                                title = "Minimum Growth Rate",
                                status = "primary",
                                solidHeader = FALSE,
                                width = 12,
                                p(class = "text-muted",
                                  paste("Select a growth rate time horizon and minimum value")),
                                selectInput("horizon", label = "Time Horizon:", 
                                            choices = c("Monthly", "Quarterly", "Annual", "5 Year", "10 Year"),
                                            selected = "Annual",
                                            selectize = FALSE),
                                sliderInput("growthQuery", "Growth Rate:", min = 0, max = .2, value = c(.05,.2))
                              )
                            ),
                            column(width = 4,
                              box(
                                title = "Geographic Filter",
                                status = "primary",
                                solidHeader = FALSE,
                                width = 12,
                                p(class = "text-muted",
                                  paste("Further refine your query by indicating level of analysis and geography")),
                                box(
                                  fluidRow(
                                    width = 6,
                                    status = "primary",
                                    solidHeader = FALSE,
                                    uiOutput("levelQueryUi")
                                  )
                                ),# end of box
                                conditionalPanel(
                                  condition = "input.analysisLevel == 2",
                                  box(
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 6,
                                    uiOutput("stateQuery2Ui")
                                  )# end of box
                                ),# end of conditional panel  
                                conditionalPanel(
                                  condition = "input.analysisLevel == 3",
                                  box(
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 6,
                                    uiOutput("stateQuery3Ui"),
                                    uiOutput("countyQuery3Ui")
                                  )# end of box
                                ),# end of conditionalpanel    
                                conditionalPanel(
                                  condition = "input.analysisLevel == 4",
                                  box(
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 6,
                                    uiOutput("stateQuery4Ui"),
                                    uiOutput("countyQuery4Ui"),
                                    uiOutput("cityQuery4Ui")
                                  )# end of box
                                )# end of conditionalpanel    
                              )#end of box
                            ) # end of column
                          ) # end of box
                        ), # end of fluidrow
                        conditionalPanel(
                          condition = "input.query",
                          fluidRow(
                            box(
                              title = "Markets by Value",
                              status = "success",
                              width = 12,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              box(
                                title = "Top Markets by Value",
                                status = "primary",
                                width = 8,
                                solidHeader = FALSE,
                                showOutput("topByValue", "polycharts")
                              ),# end of box
                              box(
                                title = "Markets by Value Table",
                                status = "primary",
                                width = 4,
                                solidHeader = FALSE,
                                dataTableOutput("valueTbl")
                              )# end of box
                            )# end of box
                          ), # end of fluidrow   
                          fluidRow(
                            box(
                              title = "Markets by Growth",
                              status = "danger",
                              width = 12,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              box(
                                title = "Top Markets by Growth",
                                status = "primary",
                                width = 8,
                                solidHeader = FALSE,
                                showOutput("topByGrowth", "polycharts")
                              ),# end of box
                              box(
                                title = "Markets by Growth Table",
                                status = "primary",
                                width = 4,
                                solidHeader = FALSE,
                                dataTableOutput("growthTbl")
                              )# end of box
                            )# end of box
                          ) # end of fluidrow   
                      ) # end of conditionalpanel
                    ) # End of fluidPage
                ), # End of tabItem 
                tabItem(tabName = "valueAnalysis",
                        fluidRow(
                          column(width = 2,
                             box(
                               status = "primary",
                               title = "Market Selector",
                               solidHeader = TRUE,
                               width = 12,
                               box(
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 uiOutput("stateQuery5Ui"),
                                 uiOutput("countyQuery5Ui"),
                                 uiOutput("cityQuery5Ui"),
                                 uiOutput("zipQuery5Ui")
                               ),# end of box
                               box(
                                 width = 12,
                                 status = "primary",
                                 solidHeader = FALSE,
                                 radioButtons("rtype", label = h4("Residence Type"),
                                              choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3, 
                                                             "4 Bedroom" = 5, "5 Bedroom" = 5, "Condo" = 6,
                                                             "Single Family Home" = 7, "All Homes" = 8), 
                                              selected = 8)
                               ),# end of box
                               actionButton("select", label = "Go")
                             )# end of box
                          ),# end of column
                          conditionalPanel(
                            condition = "input.select",
                            column(width = 10,
                              box(
                                title = "Time Series Data Exploration", status = "primary",
                                solidHeader = TRUE, height = 800, width = 12,
                                tabBox(
                                  title = "Seasonal and Non-Seasonal Time Series Decomposition",
                                  id = "exploreTab", height = 660, width = 12,
                                  tabPanel("Non-Seasonal", 
                                           box(
                                             title = "Span Order",
                                             status = "success",
                                             solidHeader = FALSE, width = 3,
                                             p(
                                               class = "text-muted",
                                               paste("Adjust span order until the simple moving average has smoothed random fluctuations
                                                     and the trend component emerges")),
                                             sliderInput("span", label = "Span Order", min = 1, max = 10, value = 3, step = 1)
                                               ),
                                           box(
                                             title = "Estimate Trend Component with Simple Moving Average (SMA)",
                                             status = "success",
                                             solidHeader = FALSE, height = 600, width = 9,
                                             plotOutput("nsPlot")
                                           )# end of box
                                  ), # end of tabPanel
                                  tabPanel("Seasonal", 
                                           box(
                                             title = "Estimate Trend Seasonal, and Irregular Components of the Time Series",
                                             status = "success",
                                             solidHeader = FALSE, height = 600, width = 12,
                                             plotOutput("tsiPlot")
                                           )
                                  )# end of tab panel
                                )# end of tabbox
                              )# end of box
                            )# end of column
                          )# end of conditional panel
                        )# end of fluidrow
                ), # end of tabItem                  
                tabItem(tabName = "trainModels",
                        column(width = 2,
                               box(
                                 status = "primary",
                                 title = "Market Selector",
                                 solidHeader = TRUE,
                                 width = 12,
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   uiOutput("stateQuery6Ui"),
                                   uiOutput("countyQuery6Ui"),
                                   uiOutput("cityQuery6Ui"),
                                   uiOutput("zipQuery6Ui")
                                 ),# end of box
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   radioButtons("rtype2", label = h4("Residence Type"),
                                                choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3, 
                                                               "4 Bedroom" = 5, "5 Bedroom" = 5, "Condo" = 6,
                                                               "Single Family Home" = 7, "All Homes" = 8), 
                                                selected = 8)
                                 )# end of box
                               )# end of box
                        ),# end of column
                        column(width = 10,
                          fluidRow(
                            box(
                              title = "Model Training Parameters",
                              status = "warning", width = 12,
                              solidHeader = TRUE,
                              box(
                                title = "Cross Validation",
                                status = "primary", width = 3,
                                solidHeader = FALSE,
                                p(
                                  class = "text-muted",
                                  paste("The time series contains median housing prices, measured monthly, from 2000 thru 2015. Here, we
                          split the time series data into training and validation sets.  Indicate here, the end year for
                          the training set. The remaining years will be used to validate the predictions.")),
                                sliderInput("split", label = "Training Set Split", min = 2004, max = 2014, value = 2014, step = 1)
                              ),# end of box
                              box(
                                title = "Model Selection",
                                status = "primary", width = 3,
                                solidHeader = FALSE,
                                p(
                                  class = "text-muted",
                                  paste("Select the forecast model algorithm.")),
                                uiOutput("modelsUi")
                              ),# end of box
                              box(
                                title = "Model Description",
                                status = "primary", width = 6,
                                solidHeader = FALSE,
                                h3(textOutput("modelNameUi")),
                                textOutput("modelDescUi"),
                                actionButton("train", label = "Run Training Model")
                              )# end of box
                            )# end of box
                        ),# end of fluidrow
                        conditionalPanel(
                          condition = "input.train",
                          fluidRow(
                            box(
                              title = "Prediction Accuracy",
                              status = "primary", width = 5,
                              solidHeader = TRUE,
                              dataTableOutput("accuracy")
                            ),# end of box
                            box(
                              title = "Training Data Prediction",
                              status = "primary", width = 7,
                              solidHeader = TRUE,
                              plotOutput("modelPlot")
                            )# end of box
                          )# end of fluidrow
                        )# end of conditional panel
                    )# end of column
                ),# end of tabItem
                tabItem(tabName = "compareModels",
                        column(width = 2,
                               box(
                                 status = "primary",
                                 title = "Market Selector",
                                 solidHeader = TRUE,
                                 width = 12,
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   uiOutput("stateQuery7Ui"),
                                   uiOutput("countyQuery7Ui"),
                                   uiOutput("cityQuery7Ui"),
                                   uiOutput("zipQuery7Ui")
                                 ),# end of box
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   radioButtons("rtype3", label = h4("Residence Type"),
                                                choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3, 
                                                               "4 Bedroom" = 5, "5 Bedroom" = 5, "Condo" = 6,
                                                               "Single Family Home" = 7, "All Homes" = 8), 
                                                selected = 8)
                                 ),# end of box
                                 actionButton("select2", label = "Go"),
                                 p(class = "text-muted",
                                   paste("Please be patient.  We are cranking through 8 training models"))
                               )# end of box
                        ),# end of column
                        conditionalPanel(
                          condition = "input.select2",
                          column(width = 10,
                                 fluidRow(
                                   box(
                                     status = "warning",
                                     width = 12,
                                     title = "Model Performance Summary",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     dataTableOutput("modelsumm")
                                   )# end of box
                                 ),#end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "Arima / ETS Model Performance",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                        box(
                                          status = "primary",
                                          width = 6,
                                          title = "Arima Model Performance",
                                          solidHeader = FALSE,
                                          plotOutput("arima")
                                        ),# end of box
                                        box(
                                          status = "primary",
                                          width = 6,
                                          title = "Exponential Smoothing (ETS) Model Performance",
                                          solidHeader = FALSE,
                                          plotOutput("ets")
                                        )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "Naive / Neural Network Model Performance",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Naive Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("naive")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Neural Network Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("neural")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "BATS / TBATS Model Performance",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "BATS Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("bats")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "TBATS Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("tbats")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "STLM / STS Model Performance",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "STLM Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("stlm")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "STS Model Performance",
                                       solidHeader = FALSE,
                                       plotOutput("sts")
                                     )# end of box
                                   )# end of box
                                 )# end of fluidrow
                          )# end of column
                    )# end of conditional panel
                ),# end of tabItem
                tabItem(tabName = "forecast",
                        column(width = 2,
                               box(
                                 status = "primary",
                                 title = "Forecast Options",
                                 solidHeader = TRUE,
                                 width = 12,
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   uiOutput("stateQuery8Ui"),
                                   uiOutput("countyQuery8Ui"),
                                   uiOutput("cityQuery8Ui"),
                                   uiOutput("zipQuery8Ui")
                                 ),# end of box
                                 box(
                                   width = 12,
                                   status = "primary",
                                   solidHeader = FALSE,
                                   radioButtons("rtype4", label = h4("Residence Type"),
                                                choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3, 
                                                               "4 Bedroom" = 5, "5 Bedroom" = 5, "Condo" = 6,
                                                               "Single Family Home" = 7, "All Homes" = 8), 
                                                selected = 8)
                                 ),# end of box
                                 box(
                                   status = "primary",
                                   title = "Years to Forecast",
                                   solidHeader = FALSE,
                                   width = 12,
                                   sliderInput("forecastRange", label = NULL, min = 1, 
                                               max = 10, value = 5),
                                   actionButton("select3", label = "Go")
                                 )# end of box
                               )# end of box
                        ),# end of column
                        conditionalPanel(
                          condition = "input.select3",
                          column(width = 10,
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "Forecast Summary",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 12,
                                       title = "Summary by Forecast Model",
                                       solidHeader = FALSE,
                                       plotOutput("forecastSummary")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "Arima / ETS Model Forecast",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Arima Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("arimaForecast")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Exponential Smoothing (ETS) Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("etsForecast")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "Naive / Neural Network Model Forecast",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Naive Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("naiveForecast")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "Neural Network Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("neuralForecast")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "BATS / TBATS Model Forecast",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "BATS Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("batsForecast")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "TBATS Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("tbatsForecast")
                                     )# end of box
                                   )# end of box
                                 ),# end of fluidrow
                                 fluidRow(
                                   box(
                                     status = "primary",
                                     width = 12,
                                     title = "STLM / STS Model Forecast",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "STLM Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("stlmForecast")
                                     ),# end of box
                                     box(
                                       status = "primary",
                                       width = 6,
                                       title = "STS Model Forecast",
                                       solidHeader = FALSE,
                                       plotOutput("stsForecast")
                                     )# end of box
                                   )# end of box
                                 )# end of fluidrow
                          )# end of column
                        )# end of conditionalpanel
                      )# end of tabItem
          ) # end of tabITems
    )# end of dashboard body
)# end of dashboard page