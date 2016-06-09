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
setwd("processedData")

# Function that reads a file based upon the name provided
readData <- function(f) {

  switch (f,
          currentZip = read.csv("currentZip.csv", header = TRUE),
          currentCity = read.csv("currentCity.csv", header = TRUE),
          currentCounty = read.csv("currentCounty.csv", header = TRUE),
          currentState = read.csv("currentState.csv", header = TRUE),
          hviAllZip = read.csv("hviAllZip.csv", header = TRUE),
          hviAllCity = read.csv("hviAllCity.csv", header = TRUE),
          hviAllCounty = read.csv("hviAllCounty.csv", header = TRUE),
          hviAllState = read.csv("hviAllState.csv", header = TRUE),
          hviCondoZip = read.csv("hviCondoZip.csv", header = TRUE),
          hviCondoCity = read.csv("hviCondoCity.csv", header = TRUE),
          hviCondoCounty = read.csv("hviCondoCounty.csv", header = TRUE),
          hviCondoState = read.csv("hviCondoState.csv", header = TRUE),
          hviSFHZip = read.csv("hviSFHZip.csv", header = TRUE),
          hviSFHCity = read.csv("hviSFHCity.csv", header = TRUE),
          hviSFHCounty = read.csv("hviSFHCounty.csv", header = TRUE),
          hviSFHState = read.csv("hviSFHState.csv", header = TRUE),
          hvi1brZip = read.csv("hvi1brZip.csv", header = TRUE),
          hvi1brCity = read.csv("hvi1brCity.csv", header = TRUE),
          hvi1brCounty = read.csv("hvi1brCounty.csv", header = TRUE),
          hvi1brState = read.csv("hvi1brState.csv", header = TRUE),
          hvi2brZip = read.csv("hvi2brZip.csv", header = TRUE),
          hvi2brCity = read.csv("hvi2brCity.csv", header = TRUE),
          hvi2brCounty = read.csv("hvi2brCounty.csv", header = TRUE),
          hvi2brState = read.csv("hvi2brState.csv", header = TRUE),
          hvi3brZip = read.csv("hvi3brZip.csv", header = TRUE),
          hvi3brCity = read.csv("hvi3brCity.csv", header = TRUE),
          hvi3brCounty = read.csv("hvi3brCounty.csv", header = TRUE),
          hvi3brState = read.csv("hvi3brState.csv", header = TRUE),
          hvi4brZip = read.csv("hvi4brZip.csv", header = TRUE),
          hvi4brCity = read.csv("hvi4brCity.csv", header = TRUE),
          hvi4brCounty = read.csv("hvi4brCounty.csv", header = TRUE),
          hvi4brState = read.csv("hvi4brState.csv", header = TRUE),
          hvi5brZip = read.csv("hvi5brZip.csv", header = TRUE),
          hvi5brCity = read.csv("hvi5brCity.csv", header = TRUE),
          hvi5brCounty = read.csv("hvi5brCounty.csv", header = TRUE),
          hvi5brState = read.csv("hvi5brState.csv", header = TRUE),
          hvisqZip = read.csv("hvisqZip.csv", header = TRUE),
          hvisqCity = read.csv("hvisqCity.csv", header = TRUE),
          hvisqCounty = read.csv("hvisqCounty.csv", header = TRUE),
          hvisqState = read.csv("hvisqState.csv", header = TRUE)
  )
}

################################################################################
#                     GLOBAL VARIABLES AND STRUCTURES                          #
################################################################################
# File containing unique geo codes, state,city, zip
geo <- read.csv("geo.csv", header = TRUE)

# Read model data
modelData <- read.xlsx("models.xlsx", sheetIndex = 1, header = TRUE)

# Read summary files
currentZip    <- readData("currentZip")
currentCity   <- readData("currentCity")
currentCounty <- readData("currentCounty")
currentState  <- readData("currentState")

# Initialize Active Data

#Default  Values
dfltState <- "Any"
dfltCounty <- "Any"
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
    current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0("$", current$Value), paste(current$State, " Home Value Index "), 
      icon = icon("dollar"), color = "green"
    )
  })
  
  #Render Monthly Price Growth  Box
  output$MonthlyUSBox <- renderValueBox({
    current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0(round(current$Monthly * 100,4), "%"), paste(current$State, 
      " Monthly Change in Home Values"), icon = icon("bullseye"), color = "orange"
    )
  })
  
  #Render Annual Price Growth  Box
  output$AnnualUSBox <- renderValueBox({
    current <- currentState[ which(currentState$State == "United States"), ]
    valueBox(
      paste0(round(current$Annual * 100,4), "%"), paste(current$State,
      " Annual Change in Home Values"), icon = icon("calendar"), color = "purple"
    )
  })
  
  #Render Top 10 States bar chart
  output$top10States <- renderChart({
    current <- currentState[ which(currentState$State != "United States"), ]
    current <- arrange(current, desc(Annual))
    current <- current[1:10,]
    p <- rPlot(x = list(var = "location", sort = "Annual"), y = "Annual", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10States', title = "Top 10 States by Annual Home Value Growth")
    p$guides(x = list(title = "State", ticks = unique(current$RegionName)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  #Render Top 10 Counties bar chart
  output$top10Counties <- renderChart({
    current <- currentCounty
    current <- arrange(current, desc(Annual))
    current <- current[1:10,]
    p <- rPlot(x = list(var = "location", sort = "Annual"), y = "Annual", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10Counties', title = "Top 10 Counties by Annual Home Value Growth")
    p$guides(x = list(title = "County", ticks = unique(current$location)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  #Render Top 10 Counties bar chart
  output$top10Cities <- renderChart({
    current <- currentCity
    current <- arrange(current, desc(Annual))
    current <- current[1:10,]
    p <- rPlot(x = list(var = "location", sort = "Annual"), y = "Annual", data = current, type = "bar")
    p$addParams(height = 300, width = 1000, dom = 'top10Cities', title = "Top 10 Cities by Annual Home Value Growth")
    p$guides(x = list(title = "City", ticks = unique(current$location)))
    p$guides(y = list(title = "Annual Growth Rate"))
    return(p)
  })
  
  
  ################################################################################
  ##                        MARKET EXPLORER FUNCTIONS                           ##
  ################################################################################
  #Level of Analysis UI
  output$levelQueryUi <- renderUI({
    radioButtons("analysisLevel", label = "Level of Analysis",
                 choices = list("State" = 1, "County" = 2, "City" = 3, "Zip"  = 4), 
                 selected = 4)
  })
  
  # State query UI
  output$stateQuery2Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state2", label = "State:", choices = c(Choose='', "Any", as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # State query UI
  output$stateQuery3Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state3", label = "State:", choices = c(Choose='', "Any", as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # State query UI
  output$stateQuery4Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state4", label = "State:", choices = c(Choose='', "Any", as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery3Ui <- renderUI({
    if (!is.null(input$state3)) {
      state <- input$state3
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county3", label = "County:", choices = c(Choose='', "Any", as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery4Ui <- renderUI({
    if (!is.null(input$stat4)) {
      state <- input$state4
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county4", label = "County:", choices = c(Choose='', "Any", as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })
  
  output$cityQuery4Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state4)) {
      if (input$state4 != "Any") {
        if (!is.null(input$county4)) {
          if (input$county4 != "Any") {
            cities  <- unique(subset(geo, StateName == input$state4 & County == input$county4, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state4, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state4, select = City))
        }
      } 
    }  
    selectInput("city4", label = "City:", choices = c(Choose='', "Any", as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })
  
  
  # Get and screen data based upon home value and growth rates
  screenData <- function() {
    # Get Deta
    d <- switch(input$analysisLevel,
                  "1" = currentState,
                  "2" = currentCounty,
                  "3" = currentCity,
                  "4" = currentZip
      )

    # Screen based upon home value index
    minValue <- input$hviQuery[1]
    if (input$maxValue == TRUE) {
      maxValue <- dfltMaxValue
    } else {
      maxValue <- input$hviQuery[2]
    }
    d <- subset(d, Value >= minValue & Value <= maxValue)

    # Screen based upon growth variable
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
           Monthly = d[ which(d$Monthly >= input$growthQuery[1]),],
           Quarterly = d[ which(d$Quarterly >= input$growthQuery[1]),],
           Annual = d[ which(d$Annual >= input$growthQuery[1]),], 
           Five = d[ which(d$Five_Year >= input$growthQuery[1]),],
           Ten = d[ which(d$Ten_Year >= input$growthQuery[1]),])
    
     return(d)
  }
  

  # Get Data for State level of analysis
  getStateData <- function() {
    
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly   = select(d, State, Value, Monthly, location),
                Quarterly = select(d, State, Value, Quarterly, location),
                Annual    = select(d, State, Value, Annual, location),
                Five      = select(d, State, Value, Five_Year, location),
                Ten       = select(d, State, Value, Ten_Year, location))

    return(d)
                
  }
  
  
  # Get Data for County level of analysis
  getCountyData <- function() {
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Filter based upon state 
    if (!is.null(input$state2) & (input$state2 != "Any")) {
      d <- d[ which(d$StateName == input$state2),]
    } 
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly   = select(d, StateName, County, Value, Monthly, location),
                Quarterly = select(d, StateName, County, Value, Quarterly, location),
                Annual    = select(d, StateName, County, Value, Annual, location),
                Five      = select(d, StateName, County, Value, Five_Year, location),
                Ten       = select(d, StateName, County, Value, Ten_Year))    
    return(d)
    
  }
  
  
  
  # Get Data for City level of analysis
  getCityData <- function() {
    
    # Get data screened by value and growth rates
    d <- screenData()
    
    # Filter based upon state and county entered
    if (!is.null(input$county3) & (input$county3 != "Any")) {
      d <- d[ which(d$County == input$county3),]
    } else if (!is.null(input$state3) & (input$state3 != "Any")) {
      d <- d[ which(d$StateName == input$state3),]
    } 
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly = select(d, StateName, County, City, Value, Monthly, location),
                Quarterly = select(d, StateName, County, City, Value, Quarterly, location),
                Annual = select(d, StateName, County, City, Value, Annual, location),
                Five = select(d, StateName, County, City, Value, Five_Year, location),
                Ten = select(d, StateName, County, City, Value, Ten_Year, location))       
    return(d)
    
  }
  
  
  
  # Get Data for Zip level of analysis
  getZipData <- function() {
    
    # Get data screened by value and growth rates
    d <- screenData()

    # Filter based upon state and county entered
    if (!is.null(input$city4) & (input$city4 != "Any")) {
      d <- d[ which(d$City == input$city4 & d$StateName == input$state4),]
    } else if (!is.null(input$county4) & (input$county4 != "Any")) {
      d <- d[ which(d$County == input$county4 & d$StateName == input$state4),]
    } else if (!is.null(input$state4) & (input$state4 != "Any")) {
      d <- d[ which(d$StateName == input$state4),]
    } 
    
    # Format growth record
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    d <- switch(horizon,
                Monthly = select(d, StateName, County, City, Zip, Value, Monthly, location),
                Quarterly = select(d, StateName, County, City, Zip, Value, Quarterly, location),
                Annual = select(d, StateName, County, City, Zip, Value, Annual, location),
                Five = select(d, StateName, County, City, Zip, Value, Five_Year, location),
                Ten = select(d, StateName, County, City, Zip, Value, Ten_Year, location))           
    
    return(d)
    
  }
  
  
  
  # Retrieves Data for Value Presentation
  getData <- eventReactive(input$query, {
    
    d <- switch(input$analysisLevel,
                "1" = getStateData(),
                "2" = getCountyData(),
                "3" = getCityData(),
                "4" = getZipData()
                )

      return(d)
  }, ignoreNULL = FALSE)


  # Get Growth Data
  getGrowthData <- eventReactive(input$query, {
    
    d <- getData()
    
    # Configure Chart based upon input horizon
    horizon <- input$horizon
    if (horizon == "5 Year") {
      horizon <- "Five"
    } else if (horizon == "10 Year") {
      horizon <- "Ten"
    }
    
    
    # Sort by horizon
    if (nrow(d) != 0) {
      d <- switch(horizon,
                  Monthly = arrange(d, desc(Monthly)),
                  Quarterly = arrange(d, desc(Quarterly)),
                  Annual = arrange(d, desc(Annual)),
                  Five = arrange(d, desc(Five_Year)),
                  Ten = arrange(d, desc(Ten_Year)))
    } 
    
    
  }, ignoreNULL = FALSE)
  
  
  #Render Top Markets by Value
  output$topByValue <- renderChart({
    
    d <- getData()
    d <- arrange(d, desc(Value))

    # Subset into top results
    numBars <- 10
    if (nrow(d) < numBars) {
      numBars <- nrow(d)
    }
    d <- d[1:numBars,]

    # Configure Chart
    p <- rPlot(x = list(var = "location", sort = "Value"), y = "Value", data = d, type = "bar")
    p$addParams(height = 300, width = 1050, dom = 'topByValue', title = paste("Top Markets by Median Home Value"))
    p$guides(x = list(title = "Market", ticks = unique(d$location)))
    p$guides(y = list(title = paste("Median Home Value")))
    return(p)
  })

  
  # Render Value Data Table
  output$valueTbl <- renderDataTable({
    d <- getData()
    d <- arrange(d, desc(Value))
    
    #Drop Location variable
    d$location <- NULL
    return(d)

  }, options = list(lengthMenu = c(5, 30, 50), autowidth = TRUE, pageLength = 5))
    

  #Render Top Markets by Growth
  output$topByGrowth <- renderChart({
    
      # Get Data
      d <- getGrowthData()
  
      # Subset into top results
      if (!is.null(d)) {
        numBars <- 10
        if (nrow(d) < numBars) {
          numBars <- nrow(d)
        }
        d <- d[1:numBars,]
      }

      # Configure Chart based upon input horizon
      horizon <- input$horizon
      if (horizon == "5 Year") {
        horizon <- "Five"
      } else if (horizon == "10 Year") {
        horizon <- "Ten"
      }
      
      p <- switch(horizon,
                  Monthly = rPlot(x = list(var = "location", sort = "Monthly"), y = "Monthly", data = d, type = "bar"),
                  Quarterly = rPlot(x = list(var = "location", sort = "Quarterly"), y = "Quarterly", data = d, type = "bar"),
                  Annual = rPlot(x = list(var = "location", sort = "Annual"), y = "Annual", data = d, type = "bar"),
                  Five = rPlot(x = list(var = "location", sort = "Five_Year"), y = "Five_Year", data = d, type = "bar"),
                  Ten = rPlot(x = list(var = "location", sort = "Ten_Year"), y = "Ten_Year", data = d, type = "bar")
      )
      p$addParams(height = 300, width = 1050, dom = 'topByGrowth', title = paste("Top Markets by ", input$horizon, " Growth"))
      p$guides(x = list(title = "Market", ticks = unique(d$location)))
      p$guides(y = list(title = paste(input$horizon,"  Growth Rate")))
      return(p)
  })

  # Render Growth Data Table
  output$growthTbl <- renderDataTable({
    
    d <- getGrowthData()

    #Drop location variable
    d$location <- NULL 
    return(d)
  }, options = list(lengthMenu = c(5, 30, 50), autowidth = TRUE, pageLength = 5))
  
  ################################################################################
  ##                        VALUE ANALYSIS FUNCTIONS                            ##
  ################################################################################
  
})