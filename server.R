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
  
  ################################################################################
  ##                        DASHBOARD SERVER FUNCTIONS                          ##
  ################################################################################
  #Render National Home Value Index Box
  output$hviUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0("$", current$Zhvi), paste(current$RegionName, " Home Value Index "), 
      icon = icon("dollar"), color = "green"
    )
  })
  
  #Render Monthly Price Growth  Box
  output$momUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0(round(current$MoM * 100,4), "%"), paste(current$RegionName, 
      " Monthly Change in Home Values"), icon = icon("bullseye"), color = "orange"
    )
  })
  
  #Render Annual Price Growth  Box
  output$yoyUSBox <- renderValueBox({
    current <- currentState[ which(currentState$RegionName == "United States"), ]
    valueBox(
      paste0(round(current$YoY * 100,4), "%"), paste(current$RegionName,
      " Annual Change in Home Values"), icon = icon("calendar"), color = "purple"
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
  
  
  ################################################################################
  ##                        MARKET EXPLORER FUNCTIONS                           ##
  ################################################################################
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

  # Get data that matches query
  filterData <- eventReactive(input$query, {
    
    # Set home value parameters
    minValue <- input$hviQuery[1]
    if (input$maxValue == TRUE) {
      maxValue <- dfltMaxValue
    } else {
      maxValue <- input$hviQuery[2]
    }

    # Filter based upon geography and home value
    if (!is.null(input$city) & input$city != "Any") {
      results <- subset(currentZip,
                        City == input$city &
                          StateName == input$state &
                          Zhvi >= minValue & Zhvi <= maxValue )
    } else if (!is.null(input$state) & input$state != "Any") {
      results <- subset(currentZip,
                        StateName == input$state &
                          Zhvi >= minValue & Zhvi <= maxValue)
    } else {
      results <- subset(currentZip,
                        Zhvi >= minValue & Zhvi <= maxValue)
    }

    return(results)
  }, ignoreNULL = FALSE)
  

  # Filter query results by requested growth horizon and rate
  getData <- eventReactive(input$query, {
    
    d <- filterData()

    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }

    switch(horizon,
           Monthly = arrange(d[ which(d$MoM >= input$growthQuery[1]),], desc(MoM)),
           Quarterly = arrange(d[ which(d$QoQ >= input$growthQuery[1]),], desc(QoQ)),
           Annual = arrange(d[ which(d$YoY >= input$growthQuery[1]),], desc(YoY)),
           Five = arrange(d[ which(d$X5Year >= input$growthQuery[1]),], desc(X5Year)),
           Ten = arrange(d[ which(d$X10Year >= input$growthQuery[1]),], desc(X10Year)))
  }, ignoreNULL = FALSE)

  
  #Render Top Markets by Value
  output$topByValue <- renderChart({
    
    # Get Data sorted by home value
    d <- arrange(getData(),desc(Zhvi))

    # Subset into top results
    numBars <- 10
    if (nrow(d) < numBars) {
      numBars <- nrow(d)
    }
    d <- d[1:numBars,]
    
    # Create location variable
    d$location <- paste0(d$City,", ",d$State," ",d$RegionName) 
    
    # Configure Chart
    p <- rPlot(x = list(var = "location", sort = "Zhvi"), y = "Zhvi", data = d, type = "bar")
    p$addParams(height = 300, width = 1050, dom = 'topByValue', title = paste("Top Markets by Median Home Value"))
    p$guides(x = list(title = "Market", ticks = unique(d$location)))
    p$guides(y = list(title = paste("Median Home Value")))
    return(p)
  })

  
  # Render Value Data Table
  output$valueTbl <- renderDataTable({
    d <- getData()

    #Sort DAta
    t <- arrange(select(d, StateName, County, City, RegionName, Zhvi), desc(Zhvi))
    colnames(t) <- c("State", "County", "City", "Zip", "Value Index")
    t
    
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    

  #Render Top Markets by Growth
  output$topByGrowth <- renderChart({
    
    # Get Data
    d <- getData()
    
    # Subset into top results
    numBars <- 10
    if (nrow(d) < numBars) {
      numBars <- nrow(d)
    }
    d <- d[1:numBars,]
    
    # Create location variable
    d$location <- paste0(d$City,", ",d$State," ",d$RegionName) 
    
    # Configure Chart based upon input horizon
    isolate({
      horizon <- input$horizon
      if (horizon == "5 Year") {
        horizon <- "Five"
      } else if (horizon == "10 Year") {
        horizon <- "Ten"
      }
      
      p <- switch(horizon,
                  Monthly = rPlot(x = list(var = "location", sort = "MoM"), y = "MoM", data = d, type = "bar"),
                  Quarterly = rPlot(x = list(var = "location", sort = "QoQ"), y = "QoQ", data = d, type = "bar"),
                  Annual = rPlot(x = list(var = "location", sort = "YoY"), y = "YoY", data = d, type = "bar"),
                  Five = rPlot(x = list(var = "location", sort = "X5Year"), y = "X5Year", data = d, type = "bar"),
                  Ten = rPlot(x = list(var = "location", sort = "X10Year"), y = "X10Year", data = d, type = "bar")
      )
      p$addParams(height = 300, width = 1050, dom = 'topByGrowth', title = paste("Top Markets by ", input$horizon, " Growth"))
      p$guides(x = list(title = "Market", ticks = unique(d$location)))
      p$guides(y = list(title = paste(input$horizon,"  Growth Rate")))
      return(p)
    })
  })
  
  # Render Growth Data Table
  output$growthTbl <- renderDataTable({
    d <- getData()

    isolate({
      horizon <- input$horizon
      if (horizon == "5 Year") {
        horizon <- "Five"
      } else if (horizon == "10 Year") {
        horizon <- "Ten"
      }
    
      t <- switch(horizon,
                  Monthly = {
                      df <- select(d, StateName, County, City, RegionName, Zhvi, MoM)
                      colnames(df) <- c("State", "County", "City", "Zip", "Value Index", "Monthly Growth")
                      t <- df 
                  },
                  Quarterly = {
                    df <- select(d, StateName, County, City, RegionName, Zhvi, QoQ)
                    colnames(df) <- c("State", "County", "City", "Zip", "Value Index", "Quarterly Growth")
                    t <- df 
                  },
                  Annual = {
                    df <- select(d, StateName, County, City, RegionName, Zhvi, YoY)
                    colnames(df) <- c("State", "County", "City", "Zip", "Value Index", "Annual Growth")
                    t <- df 
                  },
                  Five = {
                    df <- select(d, StateName, County, City, RegionName, Zhvi, X5Year)
                    colnames(df) <- c("State", "County", "City", "Zip", "Value Index", "5 Year Growth")
                    t <- df 
                  },
                  Ten = {
                    df <- select(d, StateName, County, City, RegionName, Zhvi, X10Year)
                    colnames(df) <- c("State", "County", "City", "Zip", "Value Index", "10 Year Growth")
                    t <- df 
                  }
            )
      t})
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
})