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
dfltState   <- ""
dfltCounty  <- ""
dfltCity  <- ""
dfltZip   <- ""
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
    selectInput("state2", label = "State:", choices = c(Choose='', as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # State query UI
  output$stateQuery3Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state3", label = "State:", choices = c(Choose='', as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # State query UI
  output$stateQuery4Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state4", label = "State:", choices = c(Choose='', as.character(states)), selected = dfltState, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery3Ui <- renderUI({
    if (!is.null(input$state3)) {
      if (input$state3 != "") {
        state <- input$state3
      } else {
        state <- dfltState  
      }
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county3", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })

  # County Query UI  
  output$countyQuery4Ui <- renderUI({
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        state <- input$state4
      } else {
        state <- dfltState  
      }
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county4", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })
  
  output$cityQuery4Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        if (!is.null(input$county4)) {
          if (input$county4 != "") {
            cities  <- unique(subset(geo, StateName == input$state4 & County == input$county4, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state4, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state4, select = City))
        }
      } 
    }  
    selectInput("city4", label = "City:", choices = c(Choose='', as.character(cities$City)), selected = dfltCity, selectize = FALSE)
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
    if (!is.null(input$state2) & (input$state2 != "")) {
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
    if (!is.null(input$county3) & (input$county3 != "")) {
      d <- d[ which(d$County == input$county3),]
    } else if (!is.null(input$state3) & (input$state3 != "")) {
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

    # Filter based upon state
    if (!is.null(input$state4)) {
      if (input$state4 != "") {
        d <- d[ which(d$StateName == input$state4),]
      }
    }
        
    # Filter based upon county
    if (!is.null(input$county4)) {
      if (input$county4 != "") {
        d <- d[ which(d$County == input$county4 & d$StateName == input$state4),]
      }
    }
    # Filter based upon state and county entered
    if (!is.null(input$city4)) {
      if (input$city4 != "") {
        d <- d[ which(d$City == input$city4 & d$StateName == input$state4),]
      }
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

    validate(
      need(nrow(d)>0, "No markets meet your search criteria.  Please select adjust home value range, minimum growth rate, and/or the geographic filter.")
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
  # State query UI
  output$stateQuery5Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state5", label = "State:", choices = c(Choose='', as.character(states)), selectize = FALSE)
  })
  
  # County Query UI  
  output$countyQuery5Ui <- renderUI({
    if (!is.null(input$state5)) {
      if (input$state5 != "") {
        state <- input$state5
      } else {
        state <- dfltState  
      }
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county5", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })
  

  output$cityQuery5Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state5)) {
      if (input$state5 != "") {
        if (!is.null(input$county5)) {
          if (input$county5 != "") {
            cities  <- unique(subset(geo, StateName == input$state5 & County == input$county5, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state5, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state5, select = City))
        }
      } 
    }  
    selectInput("city5", label = "City:", choices = c(Choose='', as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })
  
  
  output$zipQuery5Ui <- renderUI({

    zips <- NULL
    
    if (!is.null(input$state5)) {
      if (input$state5 != "") {
        zips <- unique(subset(geo, StateName == input$state5))
      }
    }
    
    if (!is.null(input$county5)) {
      if (input$county5 != "") {
        zips <- unique(subset(zips, StateName == input$state5 & County == input$county5)) 
      }
    }
    
    if (!is.null(input$city5)) {
      if (input$city5 != "") {
        zips <- unique(subset(zips, StateName == input$state5 & City == input$city5)) 
      }
    }
    
    
    selectInput("zip5", label = "Zip:", choices = c(Choose='', as.character(zips$RegionName)), selectize = FALSE)
    
  })

  
  
  # Get Data
  selectData <- reactive({
    level <- 0
    # Get State Data 
    if (!is.null(input$state5)) {
      if (input$state5 != "") {
        level <- "1"
        dfltState <<- input$state5
      }
    }

    if (!is.null(input$county5)) {
      if (input$county5 != "") {
        level <- "2"
        dfltCounty <<- input$county5
      }
    } 
    
    if (!is.null(input$city5)) {
      if (input$city5 != "") {
        level <- "3"
        dfltCity <<- input$city5
      }
    } 
    
    if (!is.null(input$zip5)) {
      if (input$zip5 != "") {
        level <- "4"
        dfltZip <<- input$zip5
        
      }
    } 
    if (level =="1") {
      d <- switch(input$rtype,
                  "1" = hvi1brState,
                  "2" = hvi2brState,
                  "3" = hvi3brState,
                  "4" = hvi4brState,
                  "5" = hvi5brState,
                  "6" = hviCondoState,
                  "7" = hviSFHState,
                  "8" = hviAllState)
      d <- subset(d, RegionName == input$state5, select = X2000.01:X2016.01)
    }
    
    if (level == "2") {
      d <- switch(input$rtype,
                  "1" = hvi1brCounty,
                  "2" = hvi2brCounty,
                  "3" = hvi3brCounty,
                  "4" = hvi4brCounty,
                  "5" = hvi5brCounty,
                  "6" = hviCondoCounty,
                  "7" = hviSFHCounty,
                  "8" = hviAllCounty)
      d <- subset(d, StateName == input$state5 & RegionName == input$county5, select = X2000.01:X2016.01)
    }  
    
    if (level == "3") {   
      d <- switch(input$rtype,
                  "1" = hvi1brCity,
                  "2" = hvi2brCity,
                  "3" = hvi3brCity,
                  "4" = hvi4brCity,
                  "5" = hvi5brCity,
                  "6" = hviCondoCity,
                  "7" = hviSFHCity,
                  "8" = hviAllCity)
      d <- subset(d, StateName == input$state5 & RegionName == input$city5, select = X2000.01:X2016.01)  
    }   
    
    if (level == "4") {
      d <- switch(input$rtype,
                  "1" = hvi1brZip,
                  "2" = hvi2brZip,
                  "3" = hvi3brZip,
                  "4" = hvi4brZip,
                  "5" = hvi5brZip,
                  "6" = hviCondoZip,
                  "7" = hviSFHZip,
                  "8" = hviAllZip)
      d <- subset(d, RegionName == input$zip5, select = X2000.01:X2016.01)    
    }
  return(d)
  })


    
  getTimeSeries <- eventReactive(input$select, {
    d <- selectData()
    str(d)

    if (!is.null(d)) {
      d <- as.numeric(as.vector(d))
      timeSeries <- ts(d, frequency = 12, start = c(2000,1))

      validate(
        need(!any(is.na(timeSeries)), "No markets meet your search criteria.  Please select new search criteria in the Market Selector")
      )

    return(timeSeries)
    }
  }, ignoreNULL = FALSE)
  
  
  
  # Render time series plot
  output$tsPlot <- renderPlot({
    Price <- getTimeSeries()
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render non-seasonal trend time series
  output$nsPlot <- renderPlot({
    Price <- SMA(getTimeSeries(), n = input$span)
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  # Render seasonal time series decomposition
  output$tsiPlot <- renderPlot({
    Price <- decompose(getTimeSeries())
    autoplot(Price, ts.colour = "blue") + theme_bw()
  })
  
  ################################################################################
  ##              FORECAST MODEL TRAINING FUNCTIONS                             ##
  ################################################################################
  # State query UI
  output$stateQuery6Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state6", label = "State:", choices = c(Choose='', as.character(states)), selectize = FALSE)
  })
  
  # County Query UI  
  output$countyQuery6Ui <- renderUI({
    if (!is.null(input$state6)) {
      if (input$state6 != "") {
        state <- input$state6
      } else {
        state <- dfltState  
      }
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county6", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })
  
  
  output$cityQuery6Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state6)) {
      if (input$state6 != "") {
        if (!is.null(input$county6)) {
          if (input$county6 != "") {
            cities  <- unique(subset(geo, StateName == input$state6 & County == input$county6, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state6, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state6, select = City))
        }
      } 
    }  
    selectInput("city6", label = "City:", choices = c(Choose='', as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })
  
  
  output$zipQuery6Ui <- renderUI({
    zips <- NULL
    
    if (!is.null(input$state6)) {
      if (input$state6 != "") {
        zips <- unique(subset(geo, StateName == input$state6))
      }
    }
    
    if (!is.null(input$county6)) {
      if (input$county6 != "") {
        zips <- unique(subset(zips, StateName == input$state6 & County == input$county6)) 
      }
    }
    
    if (!is.null(input$city6)) {
      if (input$city6 != "") {
        zips <- unique(subset(zips, StateName == input$state6 & City == input$city6)) 
      }
    }
    
    
    selectInput("zip6", label = "Zip:", choices = c(Choose='', as.character(zips$RegionName)), selectize = FALSE)
    
  })
  
  # Select Data for Model Training Page
  selectData2 <- function() {
    
    level <- 0
    # Get State Data 
    if (!is.null(input$state6)) {
      if (input$state6 != "") {
        level <- "1"
        dfltState <<- input$state6
      }
    }
    
    if (!is.null(input$county6)) {
      if (input$county6 != "") {
        level <- "2"
        dfltCounty <<- input$county6
      }
    } 
    
    if (!is.null(input$city6)) {
      if (input$city6 != "") {
        level <- "3"
        dfltCity <<- input$city6
      }
    } 
    
    if (!is.null(input$zip6)) {
      if (input$zip6 != "") {
        level <- "4"
        dfltZip <<- input$zip6
        
      }
    } 
    
    if (level =="1") {
      d <- switch(input$rtype2,
                  "1" = hvi1brState,
                  "2" = hvi2brState,
                  "3" = hvi3brState,
                  "4" = hvi4brState,
                  "5" = hvi5brState,
                  "6" = hviCondoState,
                  "7" = hviSFHState,
                  "8" = hviAllState)
      d <- subset(d, RegionName == input$state6, select = X2000.01:X2016.01)
    }
    
    if (level == "2") {
      d <- switch(input$rtype2,
                  "1" = hvi1brCounty,
                  "2" = hvi2brCounty,
                  "3" = hvi3brCounty,
                  "4" = hvi4brCounty,
                  "5" = hvi5brCounty,
                  "6" = hviCondoCounty,
                  "7" = hviSFHCounty,
                  "8" = hviAllCounty)
      d <- subset(d, StateName == input$state6 & RegionName == input$county6, select = X2000.01:X2016.01)
    }  
    
    if (level == "3") {   
      d <- switch(input$rtype2,
                  "1" = hvi1brCity,
                  "2" = hvi2brCity,
                  "3" = hvi3brCity,
                  "4" = hvi4brCity,
                  "5" = hvi5brCity,
                  "6" = hviCondoCity,
                  "7" = hviSFHCity,
                  "8" = hviAllCity)
      d <- subset(d, StateName == input$state6 & RegionName == input$city6, select = X2000.01:X2016.01)  
    }   
    
    if (level == "4") {
      d <- switch(input$rtype2,
                  "1" = hvi1brZip,
                  "2" = hvi2brZip,
                  "3" = hvi3brZip,
                  "4" = hvi4brZip,
                  "5" = hvi5brZip,
                  "6" = hviCondoZip,
                  "7" = hviSFHZip,
                  "8" = hviAllZip)
      d <- subset(d, RegionName == input$zip6, select = X2000.01:X2016.01)    
    }
    return(d)
  }
  
  
  # Render model select
  output$modelsUi <- renderUI({
    selectInput("model", label = "Prediction Models:", choices = c(Choose='',as.character(modelData$code)), selected = dfltModel, selectize = FALSE)
  })
  
  # Render model name
  output$modelNameUi <- renderText({
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$name)
  })
  
  # Render model description
  output$modelDescUi <- renderText({
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    paste(modelData[ which(modelData$code == m), ]$desc)  
  })
  
  
  # Split data into training and test/validation set
  splitData <- function() {
    

    if (is.null(input$split)) {
      y <- dfltSplit
    } else {
      y <- as.numeric(input$split)
    }
    
    d <- selectData2()

    # Create time series object on full data
    marketPrices  <- as.numeric(as.vector(d))
    tSeries       <- ts(marketPrices, frequency = 12, start = c(2000,1))
    
    #Split into training and test set
    tsTest  <- window(tSeries, start = c(y+1,1))
    tsTrain <- window(tSeries, end = c(y,12))
    
    #Combine into a list
    l <- list("train" = tsTrain, "test" = tsTest)
    return(l)
  }
  
  
  
  # Get plot options, specifically, number of periods to forecast and to include
  getForecastOptions <- eventReactive(input$train, {
    # Determine number of periods to forecast
    if (is.null(input$split)) {
      periods <- 12
    } else {
      periods <- (2015 - as.integer(input$split)) * 12
    }
    
    # Determine number of back periods to include
    if ((periods * 3) > (192 - periods)) {
      include <- 192 - periods
    } else {
      include <- periods * 3
    }
    
    # Determine ylimit at peak price
    maximum <- as.integer(max(selectData2()))
    
    #Combine into a list and return
    l <- list(periods = periods, include = include, maximum = maximum)
    
  }, ignoreNULL = FALSE)
  
  
  # Prepare predictions
  trainModel <- function(d) {
    if (is.null(input$model)) {
      m <- dfltModel
    } else {
      m <- input$model
    }
    
    switch (m,
            ARIMA = auto.arima(d, ic='aicc', stepwise=FALSE),
            ETS = ets(d, ic='aicc', restrict=FALSE),
            NEURAL = nnetar(d, p=12, size=25),
            TBATS = tbats(d, ic='aicc', seasonal.periods=12),
            BATS = bats(d, ic='aicc', seasonal.periods=12),
            STLM = stlm(d, s.window=12, ic='aicc', robust=TRUE, method='ets'),
            STS = StructTS(d),
            NAIVE = naive(d, getForecastOptions()$periods)
    )
  }
  
  
  #Format Accuracy Results into a table
  formatAccuracy <- function(a) {
    r <- t(a)
    measure    <-c("Mean Error (ME)",
                   "Root Mean Squared Error (RMSE)",
                   "Mean Absolute Error (MAE)",
                   "Mean Percentage Error (MPE)",
                   "Mean Absolute Percentage Error (MAPE)",
                   "Mean Absolute Scaled Error (MASE)",
                   "Autocorrelation of errors at lag 1. (ACF1)",
                   "ThEIl's U")
    trainingSet <- r[,1]
    testSet     <- r[,2]
    results     <- data.frame(measure, trainingSet, testSet)
    names(results) <- c("Measure", "Training Set", "Test Set")
    return(results)
  }
  
  # Get training forecast and test data
  getPlotData <- eventReactive(input$train, {
    
    d <- splitData()

    validate(
      need(!any(is.na(d$train)), "No markets match your selection criteria.  Please change your selection criteria in Market Selector")
    )
    m <- trainModel(d$train)
    o <- getForecastOptions()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    ma <- formatAccuracy(a)
    
    #Combine into a list
    l <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r, "accuracy" = ma)
    
    #Return list
    return(l)
  }, ignoreNULL = FALSE)
  
  
  # Render training forecast plot
  output$modelPlot <- renderPlot({
    
    p <- getPlotData()
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$accuracy <- renderDataTable({
    a <- getPlotData()
    a$accuracy
  })
  
  ################################################################################
  ##                        MODEL COMPARISON FUNCTIONS                          ##
  ################################################################################
  # State query UI
  output$stateQuery7Ui <- renderUI({
    states <- unique(geo$StateName)
    selectInput("state7", label = "State:", choices = c(Choose='', as.character(states)), selectize = FALSE)
  })
  
  # County Query UI  
  output$countyQuery7Ui <- renderUI({
    if (!is.null(input$state7)) {
      if (input$state7 != "") {
        state <- input$state7
      } else {
        state <- dfltState  
      }
    } else {
      state <- dfltState
    }
    counties <- unique(subset(geo, StateName == state, select = County))
    selectInput("county7", label = "County:", choices = c(Choose='', as.character(counties$County)), selected = dfltCounty, selectize = FALSE)
  })
  
  
  output$cityQuery7Ui <- renderUI({
    cities <- NULL
    
    if (!is.null(input$state7)) {
      if (input$state7 != "") {
        if (!is.null(input$county7)) {
          if (input$county7 != "") {
            cities  <- unique(subset(geo, StateName == input$state7 & County == input$county7, select = City))
          } else {
            cities  <- unique(subset(geo, StateName == input$state7, select = City))
          }
        } else {
          cities  <- unique(subset(geo, StateName == input$state7, select = City))
        }
      } 
    }  
    selectInput("city7", label = "City:", choices = c(Choose='', as.character(cities$City)), selected = dfltCity, selectize = FALSE)
  })
  
  
  output$zipQuery7Ui <- renderUI({
    
    zips <- NULL
    
    if (!is.null(input$state7)) {
      if (input$state7 != "") {
        zips <- unique(subset(geo, StateName == input$state7))
      }
    }
    
    if (!is.null(input$county7)) {
      if (input$county7 != "") {
        zips <- unique(subset(zips, StateName == input$state7 & County == input$county7)) 
      }
    }
    
    if (!is.null(input$city7)) {
      if (input$city7 != "") {
        zips <- unique(subset(zips, StateName == input$state7 & City == input$city7)) 
      }
    }
    
    
    selectInput("zip7", label = "Zip:", choices = c(Choose='', as.character(zips$RegionName)), selectize = FALSE)
    
  })
  
    
  # Select Data for Model Training Page
  selectData3 <- eventReactive(input$select2, {
    level <- 0
    # Get State Data 
    if (!is.null(input$state7)) {
      if (input$state7 != "") {
        level <- "1"
        dfltState <<- input$state7
      }
    }
    
    if (!is.null(input$county7)) {
      if (input$county7 != "") {
        level <- "2"
        dfltCounty <<- input$county7
      }
    } 
    
    if (!is.null(input$city7)) {
      if (input$city7 != "") {
        level <- "3"
        dfltCity <<- input$city7
      }
    } 
    
    if (!is.null(input$zip7)) {
      if (input$zip7 != "") {
        level <- "4"
        dfltZip <<- input$zip7
        
      }
    } 
    if (level =="1") {
      d <- switch(input$rtype3,
                  "1" = hvi1brState,
                  "2" = hvi2brState,
                  "3" = hvi3brState,
                  "4" = hvi4brState,
                  "5" = hvi5brState,
                  "6" = hviCondoState,
                  "7" = hviSFHState,
                  "8" = hviAllState)
      d <- subset(d, RegionName == input$state7, select = X2000.01:X2016.01)
    }
    
    if (level == "2") {
      d <- switch(input$rtype3,
                  "1" = hvi1brCounty,
                  "2" = hvi2brCounty,
                  "3" = hvi3brCounty,
                  "4" = hvi4brCounty,
                  "5" = hvi5brCounty,
                  "6" = hviCondoCounty,
                  "7" = hviSFHCounty,
                  "8" = hviAllCounty)
      d <- subset(d, StateName == input$state7 & RegionName == input$county7, select = X2000.01:X2016.01)
    }  
    
    if (level == "3") {   
      d <- switch(input$rtype3,
                  "1" = hvi1brCity,
                  "2" = hvi2brCity,
                  "3" = hvi3brCity,
                  "4" = hvi4brCity,
                  "5" = hvi5brCity,
                  "6" = hviCondoCity,
                  "7" = hviSFHCity,
                  "8" = hviAllCity)
      d <- subset(d, StateName == input$state7 & RegionName == input$city7, select = X2000.01:X2016.01)  
    }   
    
    if (level == "4") {
      d <- switch(input$rtype3,
                  "1" = hvi1brZip,
                  "2" = hvi2brZip,
                  "3" = hvi3brZip,
                  "4" = hvi4brZip,
                  "5" = hvi5brZip,
                  "6" = hviCondoZip,
                  "7" = hviSFHZip,
                  "8" = hviAllZip)
      d <- subset(d, RegionName == input$zip7, select = X2000.01:X2016.01)    
    }
    return(d)
  }, ignoreNULL = FALSE)  
  
  splitData2 <- function() {
    y <- dfltSplit

    d <- selectData3()
    validate(
      need(!any(is.na(d)), "There are no markets that match your selection criteria.  Please change your options in Market Selector")
    )
    
    # Create time series object on full data
    marketPrices  <- as.numeric(as.vector(d))
    tSeries       <- ts(marketPrices, frequency = 12, start = c(2000,1))
    
    #Split into training and test set
    tsTest  <- window(tSeries, start = c(y+1,1))
    tsTrain <- window(tSeries, end = c(y,12))
    
    #Combine into a list
    l <- list("train" = tsTrain, "test" = tsTest)
    
    return(l)
  }
  
  
  
  
  getForecastOptions2 <- eventReactive(input$select2, {
    periods <- 12
    include <- 192 - periods
    maximum <- as.integer(max(selectData3()))
    
    #Combine into a list and return
    l <- list(periods = periods, include = include, maximum = maximum)
    
    return(l)
    
  }, ignoreNULL = FALSE)
  
  
  
  output$arima <- renderPlot({
    d <- splitData2()
    m <- auto.arima(d$train, ic='aicc', stepwise=FALSE)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)

    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$ets <- renderPlot({
    d <- splitData2()
    m <- ets(d$train, ic='aicc', restrict=FALSE)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$naive <- renderPlot({
    d <- splitData2()
    m <- naive(d$train, getForecastOptions2()$periods)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })

  output$neural <- renderPlot({
    d <- splitData2()
    m <- nnetar(d$train, p=12, size=25)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  output$bats <- renderPlot({
    d <- splitData2()
    m <- bats(d$train, ic='aicc', seasonal.periods=12)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  
  output$tbats <- renderPlot({
    d <- splitData2()
    m <- tbats(d$train, ic='aicc', seasonal.periods=12)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  
  output$stlm <- renderPlot({
    d <- splitData2()
    m <- stlm(d$train, s.window=12, ic='aicc', robust=TRUE, method='ets')
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
  
  
  output$sts <- renderPlot({
    d <- splitData2()
    m <- StructTS(d$train)
    o <- getForecastOptions2()
    p <- forecast(m, o$periods)
    a <- accuracy(p, d$test)
    r <- t(a)
    
    #Combine into a plot data list
    p <- list("train" = d$train, "test" = d$test, "model" = m$model, 
              "periods" = o$periods, "maximum" = o$maximum, "include" = o$include,
              "prediction" = p, "results" = r)
    
    plot(p$prediction, include = p$include, ylim=c(0,as.numeric(p$maximum)))
    lines(p$test, col = "red")
    legend("bottomright",
           inset=.05,
           cex = 1,
           title="Legend",
           c("Actual Values","Predicted Values"),
           horiz=FALSE,
           lty=c(1,1),
           lwd=c(2,2),
           col=c("red","blue"),
           bg="white",
           text.font=3)
  })
})