# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# prePareData
library(datasets)
library(dplyr)

################################################################################
##                    GET PROESS CONTROL VARIABLES                            ##
################################################################################
today <- Sys.Date()
daysSinceLastDownload <- as.numeric(difftime(today,dateDownloaded, units = c("days")))

changed = FALSE

################################################################################
##                          DOWNLOAD DATA                                     ##
################################################################################
# Create Data Directory
if (!file.exists("data")) {
  dir.create("data")
}

if (daysSinceLastDownload == 0) { 
  
  # Current Month Data
  currentZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Summary_AllHomes.csv"
  currentCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_Summary_AllHomes.csv"
  currentCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_Summary_AllHomes.csv"
  currentStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_Summary_AllHomes.csv"
  
  download.file(currentZipURL, destfile="./data/currentZip.csv")
  download.file(currentCityURL, destfile="./data/currentCity.csv")
  download.file(currentCountyURL, destfile="./data/currentCounty.csv")
  download.file(currentStateURL, destfile="./data/currentState.csv")
  
  # All Home Price Index Time Series
  hviAllZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv"
  hviAllCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_AllHomes.csv"
  hviAllCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_AllHomes.csv"
  hviAllStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_AllHomes.csv"
  
  download.file(hviAllZipURL, destfile="./data/hviAllZip.csv")
  download.file(hviAllCityURL, destfile="./data/hviAllCity.csv")
  download.file(hviAllCountyURL, destfile="./data/hviAllCounty.csv")
  download.file(hviAllStateURL, destfile="./data/hviAllState.csv")
  
  # Condo Price Index Time Series
  hviCondoZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Condominum.csv"
  hviCondoCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_Condominum.csv"
  hviCondoCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_Condominum.csv"
  hviCondoStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_Condominum.csv"
  
  download.file(hviCondoZipURL, destfile="./data/hviCondoZip.csv")
  download.file(hviCondoCityURL, destfile="./data/hviCondoCity.csv")
  download.file(hviCondoCountyURL, destfile="./data/hviCondoCounty.csv")
  download.file(hviCondoStateURL, destfile="./data/hviCondoState.csv")
  
  # Single Family Home Price Index Time Series
  hviSFHZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_SingleFamilyResidence.csv"
  hviSFHCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_SingleFamilyResidence.csv"
  hviSFHCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_SingleFamilyResidence.csv"
  hviSFHStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_SingleFamilyResidence.csv"
  
  download.file(hviSFHZipURL, destfile="./data/hviSFHZip.csv")
  download.file(hviSFHCityURL, destfile="./data/hviSFHCity.csv")
  download.file(hviSFHCountyURL, destfile="./data/hviSFHCounty.csv")
  download.file(hviSFHStateURL, destfile="./data/hviSFHState.csv")
  
  # One Bedroom Home Price Index Time Series
  hvi1brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_1bedroom.csv"
  hvi1brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_1bedroom.csv"
  hvi1brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_1bedroom.csv"
  hvi1brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_1bedroom.csv"
  
  download.file(hvi1brZipURL, destfile="./data/hvi1brZip.csv")
  download.file(hvi1brCityURL, destfile="./data/hvi1brCity.csv")
  download.file(hvi1brCountyURL, destfile="./data/hvi1brCounty.csv")
  download.file(hvi1brStateURL, destfile="./data/hvi1brState.csv")
  
  # Two Bedroom Home Price Index Time Series
  hvi2brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_2bedroom.csv"
  hvi2brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_2bedroom.csv"
  hvi2brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_2bedroom.csv"
  hvi2brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_2bedroom.csv"
  
  download.file(hvi2brZipURL, destfile="./data/hvi2brZip.csv")
  download.file(hvi2brCityURL, destfile="./data/hvi2brCity.csv")
  download.file(hvi2brCountyURL, destfile="./data/hvi2brCounty.csv")
  download.file(hvi2brStateURL, destfile="./data/hvi2brState.csv")
  
  # Three Bedroom Home Price Index Time Series
  hvi3brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_3bedroom.csv"
  hvi3brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_3bedroom.csv"
  hvi3brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_3bedroom.csv"
  hvi3brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_3bedroom.csv"
  
  download.file(hvi3brZipURL, destfile="./data/hvi3brZip.csv")
  download.file(hvi3brCityURL, destfile="./data/hvi3brCity.csv")
  download.file(hvi3brCountyURL, destfile="./data/hvi3brCounty.csv")
  download.file(hvi3brStateURL, destfile="./data/hvi3brState.csv")
  
  # Four Bedroom Home Price Index Time Series
  hvi4brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_4bedroom.csv"
  hvi4brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_4bedroom.csv"
  hvi4brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_4bedroom.csv"
  hvi4brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_4bedroom.csv"
  
  download.file(hvi4brZipURL, destfile="./data/hvi4brZip.csv")
  download.file(hvi4brCityURL, destfile="./data/hvi4brCity.csv")
  download.file(hvi4brCountyURL, destfile="./data/hvi4brCounty.csv")
  download.file(hvi4brStateURL, destfile="./data/hvi4brState.csv")
  
  # Five Bedroom Home Price Index Time Series
  hvi5brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_5BedroomOrMore.csv"
  hvi5brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_5BedroomOrMore.csv"
  hvi5brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_5BedroomOrMore.csv"
  hvi5brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_5BedroomOrMore.csv"
  
  download.file(hvi5brZipURL, destfile="./data/hvi5brZip.csv")
  download.file(hvi5brCityURL, destfile="./data/hvi5brCity.csv")
  download.file(hvi5brCountyURL, destfile="./data/hvi5brCounty.csv")
  download.file(hvi5brStateURL, destfile="./data/hvi5brState.csv")
  
  # Home value by Square Foot
  hvisqZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_MedianValuePerSqft_AllHomes.csv"
  hvisqCityURL <- "http://files.zillowstatic.com/research/public/City/City_MedianValuePerSqft_AllHomes.csv"
  hvisqCountyURL <- "http://files.zillowstatic.com/research/public/County/County_MedianValuePerSqft_AllHomes.csv"
  hvisqStateURL <- "http://files.zillowstatic.com/research/public/State/State_MedianValuePerSqft_AllHomes.csv"
  
  download.file(hvisqZipURL, destfile="./data/hvisqZip.csv")
  download.file(hvisqCityURL, destfile="./data/hvisqCity.csv")
  download.file(hvisqCountyURL, destfile="./data/hvisqCounty.csv")
  download.file(hvisqStateURL, destfile="./data/hvisqState.csv")
  
  dateDownloaded <- Sys.Date()
  changed <- TRUE
  
}
################################################################################
##                              READ DATA                                     ##
################################################################################
if (changed == TRUE) {

  # Current Month Data
  currentZip <- read.csv("./data/currentZip.csv", header = TRUE)
  currentCity <- read.csv("./data/currentCity.csv", header = TRUE)
  currentCounty <- read.csv("./data/currentCounty.csv", header = TRUE)
  currentState <- read.csv("./data/currentState.csv", header = TRUE)
  
  
  # All Home Price Index Time Series
  hviAllZip <- read.csv("./data/hviAllZip.csv", header = TRUE)
  hviAllCity <- read.csv("./data/hviAllCity.csv", header = TRUE)
  hviAllCounty <- read.csv("./data/hviAllCounty.csv", header = TRUE)
  hviAllState <- read.csv("./data/hviAllState.csv", header = TRUE)
  
  
  # Condo Price Index Time Series
  hviCondoZip <- read.csv("./data/hviCondoZip.csv", header = TRUE)
  hviCondoCity <- read.csv("./data/hviCondoCity.csv", header = TRUE)
  hviCondoCounty <- read.csv("./data/hviCondoCounty.csv", header = TRUE)
  hviCondoState <- read.csv("./data/hviCondoState.csv", header = TRUE)
  
  
  # Single Family Home Price Index Time Series
  hviSFHZip <- read.csv("./data/hviSFHZip.csv", header = TRUE)
  hviSFHCity <- read.csv("./data/hviSFHCity.csv", header = TRUE)
  hviSFHCounty <- read.csv("./data/hviSFHCounty.csv", header = TRUE)
  hviSFHState <- read.csv("./data/hviSFHState.csv", header = TRUE)
  
  
  # One Bedroom Home Price Index Time Series
  hvi1brZip <- read.csv("./data/hvi1brZip.csv", header = TRUE)
  hvi1brCity <- read.csv("./data/hvi1brCity.csv", header = TRUE)
  hvi1brCounty <- read.csv("./data/hvi1brCounty.csv", header = TRUE)
  hvi1brState <- read.csv("./data/hvi1brState.csv", header = TRUE)
  
  
  # Two Bedroom Home Price Index Time Series
  hvi2brZip <- read.csv("./data/hvi2brZip.csv", header = TRUE)
  hvi2brCity <- read.csv("./data/hvi2brCity.csv", header = TRUE)
  hvi2brCounty <- read.csv("./data/hvi2brCounty.csv", header = TRUE)
  hvi2brState <- read.csv("./data/hvi2brState.csv", header = TRUE)
  
  
  # Three Bedroom Home Price Index Time Series
  hvi3brZip <- read.csv("./data/hvi3brZip.csv", header = TRUE)
  hvi3brCity <- read.csv("./data/hvi3brCity.csv", header = TRUE)
  hvi3brCounty <- read.csv("./data/hvi3brCounty.csv", header = TRUE)
  hvi3brState <- read.csv("./data/hvi3brState.csv", header = TRUE)
  
  
  # Four Bedroom Home Price Index Time Series
  hvi4brZip <- read.csv("./data/hvi4brZip.csv", header = TRUE)
  hvi4brCity <- read.csv("./data/hvi4brCity.csv", header = TRUE)
  hvi4brCounty <- read.csv("./data/hvi4brCounty.csv", header = TRUE)
  hvi4brState <- read.csv("./data/hvi4brState.csv", header = TRUE)
  
  
  # Five Bedroom Home Price Index Time Series
  hvi5brZip <- read.csv("./data/hvi5brZip.csv", header = TRUE)
  hvi5brCity <- read.csv("./data/hvi5brCity.csv", header = TRUE)
  hvi5brCounty <- read.csv("./data/hvi5brCounty.csv", header = TRUE)
  hvi5brState <- read.csv("./data/hvi5brState.csv", header = TRUE)
  
  
  # Home value by Square Foot
  hvisqZip <- read.csv("./data/hvisqZip.csv", header = TRUE)
  hvisqCity <- read.csv("./data/hvisqCity.csv", header = TRUE)
  hvisqCounty <- read.csv("./data/hvisqCounty.csv", header = TRUE)
  hvisqState <- read.csv("./data/hvisqState.csv", header = TRUE)
  
  dateRead <- Sys.Date()
}

################################################################################
##                          PREPROCESS DATA                                   ##
################################################################################
if (changed == TRUE) {
  
  # Combine state abbreviations and state names into a data frame.
  stateInfo <- data.frame(state.abb, state.name)
  colnames(stateInfo) <- c("State", "StateName")
  
  # Current Month Data
  print("Processing current month data")
  currentZip <- merge(currentZip, stateInfo)
  currentCity <- merge(currentCity, stateInfo)
  currentCounty <- merge(currentCounty, stateInfo)

  
  # All Home Price Index Time Series
  print("Processing a homes  data")
  hviAllZip <- merge(hviAllZip, stateInfo)
  hviAllCity <- merge(hviAllCity, stateInfo)
  hviAllCounty <- merge(hviAllCounty, stateInfo)

  
  # Condo Price Index Time Series
  print("Processing Condo data")
  hviCondoZip <- merge(hviCondoZip, stateInfo)
  hviCondoCity <- merge(hviCondoCity, stateInfo)
  hviCondoCounty <- merge(hviCondoCounty, stateInfo)

  
  # Single Family Home Price Index Time Series
  print("Processing single family home data")
  hviSFHZip <- merge(hviSFHZip, stateInfo)
  hviSFHCity <- merge(hviSFHCity, stateInfo)
  hviSFHCounty <- merge(hviSFHCounty, stateInfo)

  
  # One Bedroom Home Price Index Time Series
  print("Processing 1 bedroom data")
  hvi1brZip <- merge(hvi1brZip, stateInfo)
  hvi1brCity <- merge(hvi1brCity, stateInfo)
  hvi1brCounty <- merge(hvi1brCounty, stateInfo)

  
  # Two Bedroom Home Price Index Time Series
  print("Processing 2 bedroom data")
  hvi2brZip <- merge(hvi2brZip, stateInfo)
  hvi2brCity <- merge(hvi2brCity, stateInfo)
  hvi2brCounty <- merge(hvi2brCounty, stateInfo)

  
  # Three Bedroom Home Price Index Time Series
  print("Processing 3 bedroom data")
  hvi3brZip <- merge(hvi3brZip, stateInfo)
  hvi3brCity <- merge(hvi3brCity, stateInfo)
  hvi3brCounty <- merge(hvi3brCounty, stateInfo)

  
  # Four Bedroom Home Price Index Time Series
  print("Processing 4 bedroom data")
  hvi4brZip <- merge(hvi4brZip, stateInfo)
  hvi4brCity <- merge(hvi4brCity, stateInfo)
  hvi4brCounty <- merge(hvi4brCounty, stateInfo)

  
  # Five Bedroom Home Price Index Time Series
  print("Processing 5 bedroom data")
  hvi5brZip <- merge(hvi5brZip, stateInfo)
  hvi5brCity <- merge(hvi5brCity, stateInfo)
  hvi5brCounty <- merge(hvi5brCounty, stateInfo)

  
  # Home value by Square Foot
  print("Processing median price per sq foot data")
  hvisqZip <- merge(hvisqZip, stateInfo)
  hvisqCity <- merge(hvisqCity, stateInfo)
  hvisqCounty <- merge(hvisqCounty, stateInfo)

  #Create geo file that contains sorted state, city and zip for dropdowns.
  geo <-  select(hviAllZip, State, StateName, City, RegionName)
  geo <-  arrange(geo, State, StateName, City, RegionName)
}  

################################################################################
##                             WRITE  DATA                                    ##
################################################################################
if (changed == TRUE) {
  
  # Change working directory to data directory
  home <- getwd()
  setwd("data")
  
  # Current Month Data
  print("Writing current data")
  write.csv(currentZip, file = "currentZip.csv", row.names = FALSE, na = "")
  write.csv(currentCity, file = "currentCity.csv", row.names = FALSE, na = "")
  write.csv(currentCounty, file = "currentCounty.csv", row.names = FALSE, na = "")
  write.csv(currentState, file = "currentState.csv", row.names = FALSE, na = "")
  
  
  # All Home Price Index Time Series
  print("Writing all homes data")
  write.csv(hviAllZip, file = "hviAllZip.csv", row.names = FALSE, na = "")
  write.csv(hviAllCity, file = "hviAllCity.csv", row.names = FALSE, na = "")
  write.csv(hviAllCounty, file = "hviAllCounty.csv", row.names = FALSE, na = "")
  write.csv(hviAllState, file = "hviAllState.csv", row.names = FALSE, na = "")
  
  
  # Condo Price Index Time Series
  print("Writing condo data")
  write.csv(hviCondoZip, file = "hviCondoZip.csv", row.names = FALSE, na = "")
  write.csv(hviCondoCity, file = "hviCondoCity.csv", row.names = FALSE, na = "")
  write.csv(hviCondoCounty, file = "hviCondoCounty.csv", row.names = FALSE, na = "")
  write.csv(hviCondoState, file = "hviCondoState.csv", row.names = FALSE, na = "")
  
  
  # Single Family Home Price Index Time Series
  print("Writing all single family homes data")
  write.csv(hviSFHZip, file = "hviSFHZip.csv", row.names = FALSE, na = "")
  write.csv(hviSFHCity, file = "hviSFHCity.csv", row.names = FALSE, na = "")
  write.csv(hviSFHCounty, file = "hviSFHCounty.csv", row.names = FALSE, na = "")
  write.csv(hviSFHState, file = "hviSFHState.csv", row.names = FALSE, na = "")
  
  
  # One Bedroom Home Price Index Time Series
  print("Writing 1 bedroom home data")
  write.csv(hvi1brZip, file = "hvi1brZip.csv", row.names = FALSE, na = "")
  write.csv(hvi1brCity, file = "hvi1brCity.csv", row.names = FALSE, na = "")
  write.csv(hvi1brCounty, file = "hvi1brCounty.csv", row.names = FALSE, na = "")
  write.csv(hvi1brState, file = "hvi1brState.csv", row.names = FALSE, na = "")
  
  
  # Two Bedroom Home Price Index Time Series
  print("Writing 2 bedroom home data")
  write.csv(hvi2brZip, file = "hvi2brZip.csv", row.names = FALSE, na = "")
  write.csv(hvi2brCity, file = "hvi2brCity.csv", row.names = FALSE, na = "")
  write.csv(hvi2brCounty, file = "hvi2brCounty.csv", row.names = FALSE, na = "")
  write.csv(hvi2brState, file = "hvi2brState.csv", row.names = FALSE, na = "")
  
  
  # Three Bedroom Home Price Index Time Series
  print("Writing 3 bedroom home data")
  write.csv(hvi3brZip, file = "hvi3brZip.csv", row.names = FALSE, na = "")
  write.csv(hvi3brCity, file = "hvi3brCity.csv", row.names = FALSE, na = "")
  write.csv(hvi3brCounty, file = "hvi3brCounty.csv", row.names = FALSE, na = "")
  write.csv(hvi3brState, file = "hvi3brState.csv", row.names = FALSE, na = "")
  
  
  # Four Bedroom Home Price Index Time Series
  print("Writing 4 bedroom home data")
  write.csv(hvi4brZip, file = "hvi4brZip.csv", row.names = FALSE, na = "")
  write.csv(hvi4brCity, file = "hvi4brCity.csv", row.names = FALSE, na = "")
  write.csv(hvi4brCounty, file = "hvi4brCounty.csv", row.names = FALSE, na = "")
  write.csv(hvi4brState, file = "hvi4brState.csv", row.names = FALSE, na = "")
  
  
  # Five Bedroom Home Price Index Time Series
  print("Writing 5 bedroom home data")
  write.csv(hvi5brZip, file = "hvi5brZip.csv", row.names = FALSE, na = "")
  write.csv(hvi5brCity, file = "hvi5brCity.csv", row.names = FALSE, na = "")
  write.csv(hvi5brCounty, file = "hvi5brCounty.csv", row.names = FALSE, na = "")
  write.csv(hvi5brState, file = "hvi5brState.csv", row.names = FALSE, na = "")
  
  
  # Home value by Square Foot
  print("Writing value by square foot  data")
  write.csv(hvisqZip, file = "hvisqZip.csv", row.names = FALSE, na = "")
  write.csv(hvisqCity, file = "hvisqCity.csv", row.names = FALSE, na = "")
  write.csv(hvisqCounty, file = "hvisqCounty.csv", row.names = FALSE, na = "")
  write.csv(hvisqState, file = "hvisqState.csv", row.names = FALSE, na = "")
  
  #Write Geo File
  write.csv(geo, file = "geo.csv", row.names = FALSE, na = "")
  
   #Reset working directory
  setwd(home)
  
  changed <- FALSE
}