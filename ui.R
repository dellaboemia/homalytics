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
              dashboardHeader(title = "shinyHome"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Market Explorer", tabName = "explorer", icon = icon("search")),
                  menuItem("Value Analyzer", tabName = "valueAnalysis", icon = icon("area-chart")),
                  menuItem("Forecast Modeler", icon = icon("line-chart"),
                           menuSubItem("Train Models", icon = icon("gears"),tabName = "trainModels"),
                           menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "compareModels")),
                  menuItem("Market Forecaster", tabName = "forecast", icon = icon("bar-chart")),
                  menuItem("Help", tabName = "help", icon = icon("question-circle"),
                           menuSubItem("About shinyHome", icon = icon("user"),tabName = "helpAbout"),
                           menuSubItem("Welcome", icon = icon("coffee"),tabName = "helpWelcome"),
                           menuSubItem("Dashboard", icon = icon("dashboard"),tabName = "helpDashboard"),
                           menuItem("Market Explorer", icon = icon("search"),
                                       menuSubItem("Build a Query", icon = icon("search"), tabName = "helpBuildQuery"),
                                       menuSubItem("Market Report", icon = icon("bar-chart"), tabName = "helpMarketReport")),
                           menuItem("Value Analyzer", icon = icon("area-chart"),
                                    menuSubItem("Select a Market", icon = icon("map-marker"), tabName = "helpSelectMarket"),
                                    menuSubItem("Non-Seasonal Series", icon = icon("line-chart"), tabName = "helpNonSeasonal"),
                                    menuSubItem("Seasonal Series", icon = icon("bar-chart"), tabName = "helpSeasonal")),
                           menuItem("Forecast Modeler", icon = icon("bar-chart"),
                                    menuSubItem("Set Parameters", icon = icon("caret-square-o-right"), tabName = "helpSetParameters"),
                                    menuSubItem("Train Models", icon = icon("gears"), tabName = "helpTrainModels"),
                                    menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "helpCompareModels")),
                           menuSubItem("Market Forecaster", icon = icon("line-chart"),tabName = "helpMarketForecaster")
                           )
                )# end of sidebarMenu
              ),#end of dashboardSidebar
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidPage(
                            title = "Dashboard",
                            fluidRow(
                              column(width = 12,
                                valueBoxOutput("usViBox", width = 3),
                                valueBoxOutput("highestViBox", width = 3),
                                valueBoxOutput("usAnnualBox", width = 3),
                                valueBoxOutput("highestAnnualBox", width = 3)
                              )#end of column
                            ),# end of row
                            fluidRow(
                              column(width = 4,
                                     box(
                                       title = "Analytics for the Real Estate Market",
                                       width = 12,
                                       background = "orange",
                                       solidHeader = FALSE,
                                       collapsible = TRUE,
                                       collapsed = FALSE,
                                       h3("Welcome to shinyHome"),
                                       p(
                                         paste("Here, we use statistical inference and forecast modeling techniques to 
                                               explore and forecast over 13,000 real estate markets in the United States.  
                                               This tool will enable you to:")),
                                       tags$ul(
                                         tags$li("get a snapshot of the states, counties, and cities with the highest annual increase in median home values
                                                 on this", span("Dashboard page,", style = "color:white")),
                                         tags$li("explore home price indices and growth rates across various markets at several levels of granularity in 
                                                 the", span("Market Explorer,", style = "color:white")),
                                         tags$li("select a market and analyze and decompose price movements into their seasonal, trend and irregular components in the"
                                                 , span("Value Analyzer,", style = "color:white")),
                                         tags$li("train the most popular forecasting models and compare predictive accuracies in the", span("Forecast Modeler,", style = "color:white"), "and"),
                                         tags$li("use these models to forecast home prices in virtually every US real estate market in the", span("Market Forecaster.", 
                                                                                                                                                  style = "color:white"))
                                         ),
                                       p(
                                         paste("The menus to the left will walk you through the process of exploring markets, reviewing price trends, training 
                                               forecast models, evaluating model performance accuracy and predict home prices, 3, 5 or 10 years out.")),
                                       p(
                                         paste("To get started, click on the Market Explorer menu on the left."))
                                         )
                               ),# end of column
                              column(width = 6,
                                box(
                                  title = "Top 10 States by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  showOutput("top10States", "polycharts")
                                ),
                                box(
                                  title = "Top 10 Counties by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  showOutput("top10Counties", "polycharts")
                                ),
                                box(
                                  title = "Top 10 Cities by Annual Home Value Growth",
                                  status = "primary",
                                  width = 12,
                                  solidHeader = FALSE,
                                  collapsible = TRUE,
                                  showOutput("top10Cities", "polycharts")
                                ) #End of Box
                              ), # End of column
                              column(width = 2,
                                     fluidRow(
                                      valueBoxOutput("numStatesBox", width = 12)
                                     ),# end of fluidrow
                                     fluidRow(
                                      valueBoxOutput("numCountiesBox", width = 12)
                                     ), # end of fluidrow
                                     fluidRow(
                                      valueBoxOutput("numCitiesBox", width = 12)
                                     ),#end of fluidrow
                                     fluidRow(
                                      valueBoxOutput("numZipsBox", width = 12)
                                     )#end of fluidrow
                              )# end of column
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
                                checkboxInput("maxValue", label = "Include all values exceeding $2m", value = FALSE)
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
                                sliderInput("growthQuery", "Growth Rate:", min = 0, max = .5, value = c(.05,.5))
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
                                title = "Top Markets by Growth Table",
                                status = "primary",
                                width = 4,
                                solidHeader = FALSE,
                                dataTableOutput("growthTbl")
                              )# end of box
                            )# end of box
                          ), # end of fluidrow   
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
                                title = "Home Value Time Series Exploration", status = "primary",
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
                                actionButton("train", label = "Train Forecast Model")
                              )# end of box
                            )# end of box
                        ),# end of fluidrow
                        conditionalPanel(
                          condition = "input.train",
                          fluidRow(
                            box(
                              title = "Prediction",
                              status = "primary", width = 7,
                              solidHeader = TRUE,
                              plotOutput("modelPlot")
                            ),# end of box
                            box(
                              title = "Prediction Accuracy",
                              status = "primary", width = 5,
                              solidHeader = TRUE,
                              dataTableOutput("accuracy")
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
                      ),# end of tabItem
                tabItem(tabName = "helpAbout"
                ), #end of tabItem
                tabItem(tabName = "helpWelcome"
                ), #end of tabItem
                tabItem(tabName = "helpDashboard",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Dashboard"),
                                 p(class = "text-muted",
                                   paste("The Dashboard provides an introduction to the site and some basic 
                                         statistics on the US housing market such as:")),
                                 tags$ol(class = "text-muted",
                                   tags$li("US Home Value Index – Median Home Price"),
                                   tags$li("US Monthly Percent Change in Home Values"),
                                   tags$li("US Annual  Percent Change in Home Values"),
                                   tags$li("The top 10 states by median home value growth"),
                                   tags$li("The top 10 counties by median home value growth"),
                                   tags$li("The top 10 cities by median home value growth")
                                 )# end of tags$ol
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "dashboard.png", height = 574, width = 1020)
                                 )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpBuildQuery",
                        fluidRow(
                           box(
                             status = "primary",
                             width = 12,
                             solidHeader = FALSE,
                             img(src = "queryBuilder.png", height = 434, width = 1646)
                           )# end of box
                        ), #end of fluidrow
                        box(
                          status = "primary",
                          width = 12,
                          solidHeader = FALSE,
                          fluidRow(
                            column(width = 4,
                                   box(
                                     status = "primary",
                                     width = 12,
                                     solidHeader = FALSE,
                                     h2("Market Explorer"),
                                     p(class = "text-muted",
                                       paste("The Market Explorer ranks real estate markets by median home value and/or growth in 
                                             home value according to your query that we help you build.  The query builder enables you 
                                             to filter markets by home value range, rate of growth, and geography.")),
                                     h3("Building a Query"),
                                     h4("Setting Home Value Range"),
                                     tags$ul(class = "text-muted",
                                             tags$li("Use the Home Value Range slider to set the lower 
                                                     and upper bound on home values you wish to analyze. [1]"),
                                             tags$li("If you wish to include all homes with values over $2m, 
                                                     check the box labeled “Include all values exceeding $2m”. [2]")
                                     )# end of tags$ul
                                   )# end of box
                            ),#end of column
                            column(width = 4,
                                 box(
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   h4("Selecting Growth Rate"),
                                   p(class = "text-muted",
                                     paste("First, select the Time Horizon over which home value growth will be filtered. [3]  Options are:")),  
                                   tags$ul(class = "text-muted",
                                           tags$li("Monthly"),
                                           tags$li("Quarterly"),
                                           tags$li("Annual"),
                                           tags$li("5 Year"),
                                           tags$li("10 Year")
                                           ),# end of tags$ul
                                   p(class = "text-muted",
                                     paste("Next, slide the left side of the Growth Rate slider to the minimum growth rate 
                                           to be to be included in the analysis. [4]"))
                                 )# end of box
                            ),#end of column
                            column(width = 4,
                                   box(
                                     status = "primary",
                                     width = 12,
                                     solidHeader = FALSE,
                                     h4("Set Geographic Filter"),
                                     p(class = "text-muted",
                                       paste("The Geographic Filter enables you to select the Level of Analysis, as well as the specific 
                                             geography to analyze.")),
                                     h5("Level of Analysis"),
                                     p(class = "text-muted",
                                       paste("You can analyze markets at four levels of analysis: [5]")),
                                     tags$ul(class = "text-muted",
                                             tags$li("State – State level analysis including all 50 states"),
                                             tags$li("County – County level analysis, including one or more counties within a selected state"),
                                             tags$li("City – City level analysis, including one or more cities within a selected state or county"),
                                             tags$li("Zip – Zipcode level analysis, including one or more zipcodes, within a selected state, and county or city")
                                     ),# end of tags$ul
                                   h5("Geography"),
                                   p(class = "text-muted",
                                     paste("Once you select the level of analysis, state, county, and city selectors appear which 
                                           will allow you to further filter markets by a geography that accords with the level of 
                                           analysis. [6]")),
                                   tags$ul(class = "text-muted",
                                           tags$li("If you have selected the State level, the analysis will include all 50 states at the state level"),
                                           tags$li("If you have selected the County level, you must select the state in which the county resides"),
                                           tags$li("If you have selected the City level, you must select the state, county is optional"),
                                           tags$li("If you have selected the Zip level, you must select state and city, county is optional")
                                       ),# end of tags$ul
                                   p(class = "text-muted",
                                     paste("Once you have made your selection, press GO [7] to process your query.  Should there be no data that meets the selected criteria, you will see an error messaging 
                                       so indicating and you will be instructed to change your selection criteria in the Query Builder."))
                                   )# end of box
                            )# end of column
                          )# end of fluidRow
                        )# end of box
                ),# end of tabItem
                tabItem(tabName = "helpMarketReport",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Market Report"),
                                 h3("Markets by Growth"),
                                 h4("Top Markets by Growth Chart"),
                                 p(class = "text-muted",
                                   paste("The bar chart depicts the top 10 markets by rate of growth
                                         over the selected horizon and home value range. [1]")),
                                 h4("Top Markets by Growth Table"),
                                 p(class = "text-muted",
                                   paste("This table lists the markets by growth rate over the selected horizon.  
                                         The table also provides the median home value. [2]")),
                                 h3("Markets by Value"),
                                 h4("Top Markets by Value Chart"),
                                 p(class = "text-muted",
                                   paste("The bar chart depicts the top 10 markets by median home value
                                         over the selected horizon and home value range. [3]")),
                                 h4("Top Markets by Value Table"),
                                 p(class = "text-muted",
                                   paste("This table lists the markets by median home value over 
                                         the selected horizon.  The table also provides the growth rate information 
                                         over the selected time horizon. [4]"))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "marketReport.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpSelectMarket",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Value Analyzer"),
                                 p(class = "text-muted",
                                   paste("The Value Analyzer allows you to analyze home value price movements for selected
                                         markets over time.  You will be able to analyze and decompose price movements into their 
                                         seasonal and non-seasonal components")),
                                 h3("Select a Market"),
                                 h4("Select a Geographic Market"),
                                 p(class = "text-muted",
                                   paste(" The Market Selector enables you to select a specific market at the state, county, city,
                                         and zipcode levels. [1]")),
                                 h4("Select Residence Type"),
                                 p(class = "text-muted",
                                   paste("The analysis can be further limited to specific types of residences.  You may select
                                         1, 2, 3, 4 or 5 bedroom homes, Condos, Single Family Homes or all homes. [2]")),
                                 p(class = "text-muted",
                                   paste("Press GO to run the analysis. [3]"))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "marketSelector.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ),#end of tabItem
                tabItem(tabName = "helpNonSeasonal",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Explore Non-Seasonal Home Value Time Series"),
                                 p(class = "text-muted",
                                   paste("A non-seasonal time series consists of a trend component and an irregular component. 
                                         Decomposing the time series involves the separation of the time series into these components, 
                                         that is, estimating the trend component and the irregular component.  Here we show the trend 
                                         component by calculating the simple moving average (SMA) of the time series.")),
                                 h3("Span Order"),
                                 p(class = "text-muted",
                                   paste("To conducted a SMA, you need to specify the order (span) of the simple moving average.  
                                         Using the Span Order slider, select a span order between 1 and 10.  Through trial-and-error, 
                                         you will unveil a smooth SMA showing the trend component without excessive random fluctuation. [1]")),
                                 h3("Simple Moving Average"),
                                 p(class = "text-muted",
                                   paste("The plot shows an estimate of home value trend with a simple moving average of median home values for the market, from 2000 thru 2015. 
                                         A simple moving average (SMA) is a simple, or arithmetic, moving average that is calculated by adding the 
                                         median home value of homes in the selected market for a number of time periods and then dividing this total 
                                         by the number of time periods. Short-term averages respond quickly to changes in the home values of the underlying, 
                                         while long-term averages are slow to react. [2]"))
                               )#end of box
                        ),#end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "nonSeasonal.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                      ),#end tabItem
                tabItem(tabName = "helpSeasonal",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Explore Seasonal Home Value Time Series"),
                                 p(class = "text-muted",
                                   paste("A seasonal time series consists of a trend component, a seasonal component and an irregular component. 
                                         Decomposing the time series means separating the time series into these three components.  
                                         Click the tab labeled 'Seasonal'.  This will reveal four charts:")),
                                 tags$ul(class = "text-muted",
                                         tags$li("Observed Trend – the original time series [1]"),
                                         tags$li("Trend Component [2]"),
                                         tags$li("Seasonal Component [3]"),
                                         tags$li("Random Component [4]")
                                 )# end of tags$ul
                               )# end of box
                             ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "seasonal.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpSetParameters",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Set Model Training Parameters"),
                                 h3("Select a Market"),
                                 h4("Select a Geographic Market"),
                                 p(class = "text-muted",
                                   paste(" The Market Selector enables you to select a specific market at the state, county, city,
                                         and zipcode levels. [1]")),
                                 h4("Select Residence Type"),
                                 p(class = "text-muted",
                                   paste("The analysis can be further limited to specific types of residences.  You may select
                                         1, 2, 3, 4 or 5 bedroom homes, Condos, Single Family Homes or all homes. [2]")),
                                 h3("Select Training Parameters"),
                                 h4("Set Cross Validation"),
                                 p(class = "text-muted",
                                   paste("The home value time series data ranges from January 2000 thru December 2015.  
                                         To train the forecast algorithm, and to ascertain predictive accuracy, 
                                         the data must be split into a training set and a test set. The training 
                                         set will contain n series, starting at January 2000.  The test set will 
                                         start at series n +1 and continue through December 2015. Use the Training 
                                         Set Split slider to set the last year of the training set.  The default value is 2014. [3]")),
                                 h4("Select Forecast Algorithm"),
                                 p(class = "text-muted",
                                   paste("There are eight time series forecast algorithms available for modeling, and they are:")),
                                 tags$ul(class = "text-muted",
                                         tags$li("Arima -Autoregressive Integrated Moving Average"),
                                         tags$li("ETS – Automated Time Series Forecasting with Exponential Smoothing"),
                                         tags$li("Naïve Forecasting"),
                                         tags$li("Neural Networks – Artificial Neural Networks for Forecasting"),
                                         tags$li("BATS - Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components"),
                                         tags$li("TBATS - Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components"),
                                         tags$li("STLM - Seasonal-Trend Decomposition Procedure Based on Loess"),
                                         tags$li("STS - Basic Structural Model")
                                 ),# end of tags$ul
                                 p(class = "text-muted",
                                   paste("Use the selector to select the forecast algorithm [4], then press the 'Train Forecast Model'
                                         button to create the forecast model based upon the training set and 
                                         to calculate predictions on the test set.  [5]"))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "trainingParameters.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpTrainModels",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Train Models"),
                                 p(class = "text-muted",
                                   paste("The following exhibits include the model prediction and the prediction accuracy report.")),
                                 h3("Model Prediction"),
                                 p(class = "text-muted",
                                   paste("This plot includes the training component of the home value time series in black, 
                                         as well as a training prediction in red with the confidence interval shaded in light blue.  
                                         The dark blue line depicts actual test data.  [1]")),
                                 h3("Prediction Accuracy"),
                                 p(class = "text-muted",
                                   paste("The prediction accuracy report provides several indices for assessing prediction accuracy 
                                         on both the training and test sets.  [2]")),
                                 p(class = "text-muted",
                                   paste("To evaluate other training models, change the selection in the ‘Prediction Models’ 
                                         selector [3] and press the ‘Train Forecast Model’ button. [4]"))
                                   )# end of box
                         ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "trainModel.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column
                ), #end of tabItem
                tabItem(tabName = "helpCompareModels",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Compare Models"),
                                 p(class = "text-muted",
                                   paste("The purpose of this page is to provide you with a convenient way to train all 
                                         eight models on a market at once and to evaluate and compare performance among and 
                                         between the various forecast algorithms side-by-side.")),
                                 h3("Market Selector"),
                                 h4("Select a Geographic Market"),
                                 p(class = "text-muted",
                                   paste(" The Market Selector enables you to select a specific market at the state, county, city,
                                         and zipcode levels. [1]")),
                                 h4("Select Residence Type"),
                                 p(class = "text-muted",
                                   paste("The analysis can be further limited to specific types of residences.  You may select
                                         1, 2, 3, 4 or 5 bedroom homes, Condos, Single Family Homes or all homes. [2]")),
                                 p(class = "text-muted",
                                   paste("Press GO to run the analysis. [3]")),
                                 h3("Model Performance Summary"),
                                 p(class = "text-muted",
                                   paste("The prediction accuracy measures are calculated for each forecast algorithm and are presented 
                                         in a sortable datatable format.  To rank the forecast algorithms by a specific error statistic, 
                                         click on the caret next to the column heading for the error statistic. [4]")),
                                 h3("Model Performance"),
                                 p(class = "text-muted",
                                   paste("The rest of the page presents the prediction plots for each of the eight forecast models 
                                         in a 4 by 2 arrangement. [5]"))
                               )# end of box
                        ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "modelCompare.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column                        
                ), #end of tabItem
                tabItem(tabName = "helpMarketForecaster",
                        column(width = 4,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 h2("Market Forecaster"),
                                 p(class = "text-muted",
                                   paste("This page enables you to create between 1 and 10 year home value forecasts for your selected market.  
                                         Forecasts produced by each of the eight forecast models are presented with confidence intervals 
                                         for comparative purposes.")),
                                 h3("Forecast Options"),
                                 h4("Select a Geographic Market"),
                                 p(class = "text-muted",
                                   paste(" The Market Selector enables you to select a specific market at the state, county, city,
                                         and zipcode levels. [1]")),
                                 h4("Select Residence Type"),
                                 p(class = "text-muted",
                                   paste("The analysis can be further limited to specific types of residences.  You may select
                                         1, 2, 3, 4 or 5 bedroom homes, Condos, Single Family Homes or all homes. [2]")),
                                 h4("Select Years to Forecast"),
                                 p(class = "text-muted",
                                   paste("Use the ‘Years to Forecast’ slider to forecast home values from 1 to 10 years. [3]")),
                                 p(class = "text-muted",
                                   paste("Press GO to run the analysis. [4]")),
                                 h3("Forecast Summary"),
                                 p(class = "text-muted",
                                   paste("The forecast summary plots the forecasts for each of the eight forecast models on a single plot. [5]")),
                                 h3("Model Forecasts"),
                                 p(class = "text-muted",
                                   paste("The rest of the page contains forecasts for each of the eight forecast models independently 
                                         in a 4 by 2 arrangement. [6]"))
                                   )# end of box
                                   ),# end of column
                        column(width = 8,
                               box(
                                 status = "primary",
                                 width = 12,
                                 solidHeader = FALSE,
                                 img(src = "marketForecast.png", height = 574, width = 1020)
                               )# end of box
                        )# end of column                        
                 ) #end of tabItem
          ) # end of tabITems
    )# end of dashboard body
)# end of dashboard page