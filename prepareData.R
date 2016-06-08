# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# prePareData
library(datasets)
library(data.table)
library(dplyr)

################################################################################
##                          CONTROL FLAGS                                     ##
################################################################################
downloadData <- FALSE
readData <- TRUE


################################################################################
##                          DOWNLOAD DATA                                     ##
################################################################################
home <- getwd()
# Create Data Directory
if (!file.exists("rawData")) {
  dir.create("rawData")
}

setwd("rawData")

if (downloadData == TRUE) {
  # Current Month Data
  currentZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Summary_AllHomes.csv"
  currentCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_Summary_AllHomes.csv"
  currentCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_Summary_AllHomes.csv"
  currentStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_Summary_AllHomes.csv"
  
  download.file(currentZipURL, destfile="currentZip.csv")
  download.file(currentCityURL, destfile="currentCity.csv")
  download.file(currentCountyURL, destfile="currentCounty.csv")
  download.file(currentStateURL, destfile="currentState.csv")
  
  # All Home Price Index Time Series
  hviAllZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv"
  hviAllCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_AllHomes.csv"
  hviAllCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_AllHomes.csv"
  hviAllStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_AllHomes.csv"
  
  download.file(hviAllZipURL, destfile="hviAllZip.csv")
  download.file(hviAllCityURL, destfile="hviAllCity.csv")
  download.file(hviAllCountyURL, destfile="hviAllCounty.csv")
  download.file(hviAllStateURL, destfile="hviAllState.csv")
  
  # Condo Price Index Time Series
  hviCondoZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_Condominum.csv"
  hviCondoCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_Condominum.csv"
  hviCondoCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_Condominum.csv"
  hviCondoStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_Condominum.csv"
  
  download.file(hviCondoZipURL, destfile="hviCondoZip.csv")
  download.file(hviCondoCityURL, destfile="hviCondoCity.csv")
  download.file(hviCondoCountyURL, destfile="hviCondoCounty.csv")
  download.file(hviCondoStateURL, destfile="hviCondoState.csv")
  
  # Single Family Home Price Index Time Series
  hviSFHZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_SingleFamilyResidence.csv"
  hviSFHCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_SingleFamilyResidence.csv"
  hviSFHCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_SingleFamilyResidence.csv"
  hviSFHStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_SingleFamilyResidence.csv"
  
  download.file(hviSFHZipURL, destfile="hviSFHZip.csv")
  download.file(hviSFHCityURL, destfile="hviSFHCity.csv")
  download.file(hviSFHCountyURL, destfile="hviSFHCounty.csv")
  download.file(hviSFHStateURL, destfile="hviSFHState.csv")
  
  # One Bedroom Home Price Index Time Series
  hvi1brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_1bedroom.csv"
  hvi1brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_1bedroom.csv"
  hvi1brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_1bedroom.csv"
  hvi1brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_1bedroom.csv"
  
  download.file(hvi1brZipURL, destfile="hvi1brZip.csv")
  download.file(hvi1brCityURL, destfile="hvi1brCity.csv")
  download.file(hvi1brCountyURL, destfile="hvi1brCounty.csv")
  download.file(hvi1brStateURL, destfile="hvi1brState.csv")
  
  # Two Bedroom Home Price Index Time Series
  hvi2brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_2bedroom.csv"
  hvi2brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_2bedroom.csv"
  hvi2brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_2bedroom.csv"
  hvi2brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_2bedroom.csv"
  
  download.file(hvi2brZipURL, destfile="hvi2brZip.csv")
  download.file(hvi2brCityURL, destfile="hvi2brCity.csv")
  download.file(hvi2brCountyURL, destfile="hvi2brCounty.csv")
  download.file(hvi2brStateURL, destfile="hvi2brState.csv")
  
  # Three Bedroom Home Price Index Time Series
  hvi3brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_3bedroom.csv"
  hvi3brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_3bedroom.csv"
  hvi3brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_3bedroom.csv"
  hvi3brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_3bedroom.csv"
  
  download.file(hvi3brZipURL, destfile="hvi3brZip.csv")
  download.file(hvi3brCityURL, destfile="hvi3brCity.csv")
  download.file(hvi3brCountyURL, destfile="hvi3brCounty.csv")
  download.file(hvi3brStateURL, destfile="hvi3brState.csv")
  
  # Four Bedroom Home Price Index Time Series
  hvi4brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_4bedroom.csv"
  hvi4brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_4bedroom.csv"
  hvi4brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_4bedroom.csv"
  hvi4brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_4bedroom.csv"
  
  download.file(hvi4brZipURL, destfile="hvi4brZip.csv")
  download.file(hvi4brCityURL, destfile="hvi4brCity.csv")
  download.file(hvi4brCountyURL, destfile="hvi4brCounty.csv")
  download.file(hvi4brStateURL, destfile="hvi4brState.csv")
  
  # Five Bedroom Home Price Index Time Series
  hvi5brZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_5BedroomOrMore.csv"
  hvi5brCityURL <- "http://files.zillowstatic.com/research/public/City/City_Zhvi_5BedroomOrMore.csv"
  hvi5brCountyURL <- "http://files.zillowstatic.com/research/public/County/County_Zhvi_5BedroomOrMore.csv"
  hvi5brStateURL <- "http://files.zillowstatic.com/research/public/State/State_Zhvi_5BedroomOrMore.csv"
  
  download.file(hvi5brZipURL, destfile="hvi5brZip.csv")
  download.file(hvi5brCityURL, destfile="hvi5brCity.csv")
  download.file(hvi5brCountyURL, destfile="hvi5brCounty.csv")
  download.file(hvi5brStateURL, destfile="hvi5brState.csv")
  
  # Home value by Square Foot
  hvisqZipURL <- "http://files.zillowstatic.com/research/public/Zip/Zip_MedianValuePerSqft_AllHomes.csv"
  hvisqCityURL <- "http://files.zillowstatic.com/research/public/City/City_MedianValuePerSqft_AllHomes.csv"
  hvisqCountyURL <- "http://files.zillowstatic.com/research/public/County/County_MedianValuePerSqft_AllHomes.csv"
  hvisqStateURL <- "http://files.zillowstatic.com/research/public/State/State_MedianValuePerSqft_AllHomes.csv"
  
  download.file(hvisqZipURL, destfile="hvisqZip.csv")
  download.file(hvisqCityURL, destfile="hvisqCity.csv")
  download.file(hvisqCountyURL, destfile="hvisqCounty.csv")
  download.file(hvisqStateURL, destfile="hvisqState.csv")

}
  
################################################################################
##                              READ DATA                                     ##
################################################################################

if (readData == TRUE) {
  # Current Month Data
  print("Reading current data")
  currentZip <- read.csv("currentZip.csv", header = TRUE)
  currentCity <- read.csv("currentCity.csv", header = TRUE)
  currentCounty <- read.csv("currentCounty.csv", header = TRUE)
  currentState <- read.csv("currentState.csv", header = TRUE)
  
  
  # All Home Price Index Time Series
  print("Reading All Home data")
  hviAllZip <- read.csv("hviAllZip.csv", header = TRUE)
  hviAllCity <- read.csv("hviAllCity.csv", header = TRUE)
  hviAllCounty <- read.csv("hviAllCounty.csv", header = TRUE)
  hviAllState <- read.csv("hviAllState.csv", header = TRUE)
  
  
  # Condo Price Index Time Series
  print("Reading Condo data")
  hviCondoZip <- read.csv("hviCondoZip.csv", header = TRUE)
  hviCondoCity <- read.csv("hviCondoCity.csv", header = TRUE)
  hviCondoCounty <- read.csv("hviCondoCounty.csv", header = TRUE)
  hviCondoState <- read.csv("hviCondoState.csv", header = TRUE)
  
  
  # Single Family Home Price Index Time Series
  print("Reading single family home data")
  hviSFHZip <- read.csv("hviSFHZip.csv", header = TRUE)
  hviSFHCity <- read.csv("hviSFHCity.csv", header = TRUE)
  hviSFHCounty <- read.csv("hviSFHCounty.csv", header = TRUE)
  hviSFHState <- read.csv("hviSFHState.csv", header = TRUE)
  
  
  # One Bedroom Home Price Index Time Series
  print("Reading 1 bedroom data")
  hvi1brZip <- read.csv("hvi1brZip.csv", header = TRUE)
  hvi1brCity <- read.csv("hvi1brCity.csv", header = TRUE)
  hvi1brCounty <- read.csv("hvi1brCounty.csv", header = TRUE)
  hvi1brState <- read.csv("hvi1brState.csv", header = TRUE)
  
  
  # Two Bedroom Home Price Index Time Series
  print("Reading 2 bedroom data")
  hvi2brZip <- read.csv("hvi2brZip.csv", header = TRUE)
  hvi2brCity <- read.csv("hvi2brCity.csv", header = TRUE)
  hvi2brCounty <- read.csv("hvi2brCounty.csv", header = TRUE)
  hvi2brState <- read.csv("hvi2brState.csv", header = TRUE)
  
  
  # Three Bedroom Home Price Index Time Series
  print("Reading 3 bedroom data")
  hvi3brZip <- read.csv("hvi3brZip.csv", header = TRUE)
  hvi3brCity <- read.csv("hvi3brCity.csv", header = TRUE)
  hvi3brCounty <- read.csv("hvi3brCounty.csv", header = TRUE)
  hvi3brState <- read.csv("hvi3brState.csv", header = TRUE)
  
  
  # Four Bedroom Home Price Index Time Series
  print("Reading 4 bedroom data")
  hvi4brZip <- read.csv("hvi4brZip.csv", header = TRUE)
  hvi4brCity <- read.csv("hvi4brCity.csv", header = TRUE)
  hvi4brCounty <- read.csv("hvi4brCounty.csv", header = TRUE)
  hvi4brState <- read.csv("hvi4brState.csv", header = TRUE)
  
  
  # Five Bedroom Home Price Index Time Series
  print("Reading 5 bedroom data")
  hvi5brZip <- read.csv("hvi5brZip.csv", header = TRUE)
  hvi5brCity <- read.csv("hvi5brCity.csv", header = TRUE)
  hvi5brCounty <- read.csv("hvi5brCounty.csv", header = TRUE)
  hvi5brState <- read.csv("hvi5brState.csv", header = TRUE)
  
  
  # Home value by Square Foot
  print("Reading value by sq foot data")
  hvisqZip <- read.csv("hvisqZip.csv", header = TRUE)
  hvisqCity <- read.csv("hvisqCity.csv", header = TRUE)
  hvisqCounty <- read.csv("hvisqCounty.csv", header = TRUE)
  hvisqState <- read.csv("hvisqState.csv", header = TRUE)
}


################################################################################
##                          PREPROCESS DATA                                   ##
################################################################################
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
geo <-  select(currentZip, State, StateName, County, City, RegionName)
geo <-  arrange(geo, State, StateName, County, City, RegionName)

## Create location variable
print("Creating location variable")
currentZip$location    <- paste0(currentZip$City, ", ", currentZip$State, " ", currentZip$RegionName)
currentCity$location   <- paste0(currentCity$RegionName, ", ", currentCity$State)
currentCounty$location <- paste0(currentCounty$RegionName, ", ", currentCounty$State)
currentState$location  <- paste0(currentState$RegionName)

## Change RegionName variable
print("Change RegionName variable")
setnames(currentZip, "RegionName", "Zip")
setnames(currentCity, "RegionName", "City")
setnames(currentCounty, "RegionName", "County")
setnames(currentState, "RegionName", "State")

## Change Monthly Growth variable
print("Change Monthly Growth variable")
setnames(currentZip, "MoM", "Monthly")
setnames(currentCity, "MoM", "Monthly")
setnames(currentCounty, "MoM", "Monthly")
setnames(currentState, "MoM", "Monthly")

## Change Quarterly Growth variable
print("Change Quarterly Growth variable")
setnames(currentZip, "QoQ", "Quarterly")
setnames(currentCity, "QoQ", "Quarterly")
setnames(currentCounty, "QoQ", "Quarterly")
setnames(currentState, "QoQ", "Quarterly")

## Change Annual Growth variable
print("Change Annual Growth variable")
setnames(currentZip, "YoY", "Annual")
setnames(currentCity, "YoY", "Annual")
setnames(currentCounty, "YoY", "Annual")
setnames(currentState, "YoY", "Annual")

## Change 5 Year Growth variable
print("Change 5 Year Growth variable")
setnames(currentZip, "X5Year", "Five_Year")
setnames(currentCity, "X5Year", "Five_Year")
setnames(currentCounty, "X5Year", "Five_Year")
setnames(currentState, "X5Year", "Five_Year")

## Change 10 Year Growth variable
print("Change 10 Year Growth variable")
setnames(currentZip, "X10Year", "Ten_Year")
setnames(currentCity, "X10Year", "Ten_Year")
setnames(currentCounty, "X10Year", "Ten_Year")
setnames(currentState, "X10Year", "Ten_Year")

## Change Value variable
print("Change 10 Year Growth variable")
setnames(currentZip, "Zhvi", "Value")
setnames(currentCity, "Zhvi", "Value")
setnames(currentCounty, "Zhvi", "Value")
setnames(currentState, "Zhvi", "Value")


################################################################################
##                             WRITE  DATA                                    ##
################################################################################
# Change working directory to data directory
setwd(home)

# Create Data Directory
if (!file.exists("processedData")) {
  dir.create("processedData")
}

setwd("processedData")

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