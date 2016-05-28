# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# server.R

library(datasets)
library(dplyr)
library(forecast)
library(ggplot2)
library(plyr)
library(rCharts)
library(shiny)
library(TTR)
library(xlsx)

################################################################################
#                                READ DATA                                     #
################################################################################
# Function that reads a file based upon the name provided
readData <- function(f) {
  
  switch (f,
          currentZip = read.csv("./data/currentZip.csv", header = TRUE),
          currentCity = read.csv("./data/currentCity.csv", header = TRUE),
          currentCounty = read.csv("./data/currentCounty.csv", header = TRUE),
          currentState = read.csv("./data/currentState.csv", header = TRUE),
          hviAllZip = read.csv("./data/hviAllZip.csv", header = TRUE),
          hviAllCity = read.csv("./data/hviAllCity.csv", header = TRUE),
          hviAllCounty = read.csv("./data/hviAllCounty.csv", header = TRUE),
          hviAllState = read.csv("./data/hviAllState.csv", header = TRUE),
          hviCondoZip = read.csv("./data/hviCondoZip.csv", header = TRUE),
          hviCondoCity = read.csv("./data/hviCondoCity.csv", header = TRUE),
          hviCondoCounty = read.csv("./data/hviCondoCounty.csv", header = TRUE),
          hviCondoState = read.csv("./data/hviCondoState.csv", header = TRUE),
          hviSFHZip = read.csv("./data/hviSFHZip.csv", header = TRUE),
          hviSFHCity = read.csv("./data/hviSFHCity.csv", header = TRUE),
          hviSFHCounty = read.csv("./data/hviSFHCounty.csv", header = TRUE),
          hviSFHState = read.csv("./data/hviSFHState.csv", header = TRUE),
          hvi1brZip = read.csv("./data/hvi1brZip.csv", header = TRUE),
          hvi1brCity = read.csv("./data/hvi1brCity.csv", header = TRUE),
          hvi1brCounty = read.csv("./data/hvi1brCounty.csv", header = TRUE),
          hvi1brState = read.csv("./data/hvi1brState.csv", header = TRUE),
          hvi2brZip = read.csv("./data/hvi2brZip.csv", header = TRUE),
          hvi2brCity = read.csv("./data/hvi2brCity.csv", header = TRUE),
          hvi2brCounty = read.csv("./data/hvi2brCounty.csv", header = TRUE),
          hvi2brState = read.csv("./data/hvi2brState.csv", header = TRUE),
          hvi3brZip = read.csv("./data/hvi3brZip.csv", header = TRUE),
          hvi3brCity = read.csv("./data/hvi3brCity.csv", header = TRUE),
          hvi3brCounty = read.csv("./data/hvi3brCounty.csv", header = TRUE),
          hvi3brState = read.csv("./data/hvi3brState.csv", header = TRUE),
          hvi4brZip = read.csv("./data/hvi4brZip.csv", header = TRUE),
          hvi4brCity = read.csv("./data/hvi4brCity.csv", header = TRUE),
          hvi4brCounty = read.csv("./data/hvi4brCounty.csv", header = TRUE),
          hvi4brState = read.csv("./data/hvi4brState.csv", header = TRUE),
          hvi5brZip = read.csv("./data/hvi5brZip.csv", header = TRUE),
          hvi5brCity = read.csv("./data/hvi5brCity.csv", header = TRUE),
          hvi5brCounty = read.csv("./data/hvi5brCounty.csv", header = TRUE),
          hvi5brState = read.csv("./data/hvi5brState.csv", header = TRUE),
          hvisqZip = read.csv("./data/hvisqZip.csv", header = TRUE),
          hvisqCity = read.csv("./data/hvisqCity.csv", header = TRUE),
          hvisqCounty = read.csv("./data/hvisqCounty.csv", header = TRUE),
          hvisqState = read.csv("./data/hvisqState.csv", header = TRUE)
  )
}

################################################################################
#                     GLOBAL VARIABLES AND STRUCTURES                          #
################################################################################
# File containing unique geo codes, state,city, zip
geo <- read.csv("./data/geo.csv", header = TRUE)

# Read model data
modelData <- read.xlsx("./data/models.xlsx", sheetIndex = 1, header = TRUE)

# Read summary files
currentZip    <- readData("currentZip")
currentCity   <- readData("currentCity")
currentCounty <- readData("currentCounty")
currentState  <- readData("currentState")

#Default  Values
dfltState <- "Any"
dfltCity  <- "Any"
dfltZip   <- 94133
dfltModel <- "ARIMA"
dfltSplit <- 2014
dfltMaxValue <- 10000000
################################################################################
#                               SHINYSERVER                                    #
################################################################################

shinyServer(function(input, output) {
  
  #Render National Home Value Index Box
  output$hviUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0("$", current$Zhvi), paste(current$RegionName, " Home Value Index "), icon = icon("dollar"), color = "green"
    )
  })
  
  #Render Monthly Price Growth  Box
  output$momUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0(round(current$MoM * 100,4), "%"), paste(current$RegionName, " Monthly Change in Home Values"), icon = icon("bullseye"), color = "orange"
    )
  })
  
  #Render Annual Price Growth  Box
  output$yoyUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0(round(current$YoY * 100,4), "%"), paste(current$RegionName, " Annual Change in Home Values"), icon = icon("calendar"), color = "purple"
    )
  })
  
  #Render Top 10 States bar chart
  output$top10States <- renderChart({
    current <- currentState[ which(currentState$RegionName != "United States"), ]
    current <- arrange(current, desc(YoY))
    current <- current[1:10,]
    p <- rPlot(x = list(var = "RegionName", sort = "YoY"), y = "YoY", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10States', title = "Top 10 States by Annual Home Value Growth")
    p$guides(x = list(title = "State", ticks = unique(current$RegionName)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  #Render Top 10 Counties bar chart
  output$top10Counties <- renderChart({
    current <- currentCounty
    current <- arrange(current, desc(YoY))
    current <- current[1:10,]
    current$location <- paste(current$RegionName, ", ", current$State)
    p <- rPlot(x = list(var = "location", sort = "YoY"), y = "YoY", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10Counties', title = "Top 10 Counties by Annual Home Value Growth")
    p$guides(x = list(title = "County", ticks = unique(current$location)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  #Render Top 10 Counties bar chart
  output$top10Cities <- renderChart({
    current <- currentCity
    current <- arrange(current, desc(YoY))
    current <- current[1:10,]
    current$location <- paste(current$RegionName, ", ", current$State)
    p <- rPlot(x = list(var = "location", sort = "YoY"), y = "YoY", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10Cities', title = "Top 10 Cities by Annual Home Value Growth")
    p$guides(x = list(title = "City", ticks = unique(current$location)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  # State query UI
  output$stateQueryUi <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state", label = "State:", choices = c(Choose='', "Any", as.character(states)), selected = dfltState, selectize = FALSE)
  })
  
  # State query UI
  output$cityQueryUi <- renderUI({
    if (!is.null(input$state)) {
      state <- input$state
    } else {
      state <- dfltState
    }
    cities <- unique(subset(geo, StateName == state, select = City))
    selectInput("city", label = "City:", choices = c(Choose='', "Any", as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })

  # Filter query results by requested growth horizon and rate
  filterByGrowth <- function(d) {

    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }

    print(paste("filtering by ", horizon, " value ", input$growthQuery[1]))
    print(paste("Rows passed to be filtered", nrow(d)))
    print(paste("class of growth is ", class(input$growthQuery[1])))
    
    switch(horizon,
           Monthly = arrange(d[ which(d$MoM >= input$growthQuery[1]),], desc(MoM)),
           Quarterly = arrange(d[ which(d$QoQ >= input$growthQuery[1]),], desc(QoQ)),
           Annual = arrange(d[ which(d$YoY >= input$growthQuery[1]),], desc(YoY)),
           Five = arrange(d[ which(d$X5Year >= input$growthQuery[1]),], desc(X5Year)),
           Ten = arrange(d[ which(d$X10Year >= input$growthQuery[1]),], desc(X10Year)))
  }

  # Get data that matches query
  getData <- eventReactive(input$query, {
  
    # Set home value parameters
    minValue <- input$hviQuery[1]
    if (input$maxValue == TRUE) {
      maxValue <- dfltMaxValue
    } else {
      maxValue <- input$hviQuery[2]
    }
    print(paste("Horizon is:", input$horizon))
    print(paste("Minimum Home Value :", input$hviQuery[1]))
    print(paste("Maximum Home Value :", maxValue))
    print(paste("Minimum Growth Rate :", input$growthQuery[1]))
    print(paste("State :", input$state))
    print(paste("City :", input$city))

    # Filter based upon geography and home value
    if (!is.null(input$city) & input$city != "Any") {
      print("...getting results for city...")
      results <- subset(currentZip,
                          City == input$city &
                          StateName == input$state &
                          Zhvi >= minValue & Zhvi <= maxValue )
      print(paste("...dimension of results is ", dim(results)))
    } else if (!is.null(input$state) & input$state != "Any") {
      print("...getting results for state...")
      results <- subset(currentZip,
                          StateName == input$state &
                          Zhvi >= minValue & Zhvi <= maxValue)
      print(paste("...dimension of results is ", dim(results)))    
      } else {
      print("...getting all results..")
      results <- subset(currentZip,
                        Zhvi >= minValue & Zhvi <= maxValue)
      print(paste("...dimension of results is ", dim(results)))
    }

    print(paste("Query returned ", nrow(results), " results."))

    return(isolate(filterByGrowth(results)))
  })
  
  #Set core bar plot parameters
  setCorePlot <- function(d) {
    
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    
    switch(horizon,
           Monthly = rPlot(x = list(var = "location", sort = "MoM"), y = "MoM", data = d, type = "bar"),
           Quarterly = rPlot(x = list(var = "location", sort = "QoQ"), y = "QoQ", data = d, type = "bar"),
           Annual = rPlot(x = list(var = "location", sort = "YoY"), y = "YoY", data = d, type = "bar"),
           Five = rPlot(x = list(var = "location", sort = "X5Year"), y = "X5Year", data = d, type = "bar"),
           Ten = rPlot(x = list(var = "location", sort = "X10Year"), y = "X10Year", data = d, type = "bar")
    )
  }

  #Render Top 10 Markets Meeting Criteria
  output$topByGrowth <- renderChart({
    d <- getData()
    if (is.null(d)) {
      print("Query returned zero rows")
      p <- NULL
    } else {
      print(paste("Filtered query returned ", nrow(d), " results"))
      numBars <- 10
      if (nrow(d) < 10) {
        numBars <- nrow(d)
      }
      d <- d[1:numBars,]
      d$location <- paste(d$City, ", ", d$State, " ", d$RegionName)
      p <- setCorePlot(d)
      p$addParams(height = 300, width = 1500, dom = 'topByGrowth', title = paste("Top Markets by ", input$horizon, " Growth"))
      p$guides(x = list(title = "Market", ticks = unique(d$location)))
      p$guides(y = list(title = paste(input$horizon,"  Growth Rate")))
    }  
    return(p)
  })
  
})