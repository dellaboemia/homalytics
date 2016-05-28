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
                  menuItem("Value Analysis", tabName = "exploreData", icon = icon("area-chart")),
                  menuItem("Forecast Models", icon = icon("line-chart"),
                           menuSubItem("Model Training", icon = icon("gears"),tabName = "trainModels"),
                           menuSubItem("Model Performance", icon = icon("check-circle"))),
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
                              valueBoxOutput("momUSBox"),
                              valueBoxOutput("yoyUSBox")
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
                              ) # end of box
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
                                  paste("Further refine your query by indicating state and city")),
                                uiOutput("stateQueryUi"),
                                uiOutput("cityQueryUi"),
                                actionButton("query", label = "Go")
                              )# end of box
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
                ) # End of tabItem 
          ) # end of tabITems
    )# end of dashboard body
)# end of dashboard page