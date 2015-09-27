getProjectData <- function(){
  dataUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
  zippedFile <- 'data/exdata-data-NEI_data.zip'
  dataClassificationFile <- 'data/Source_Classification_Code.rds'
  dataSummaryFile <- 'data/summarySCC_PM25.rds'
  
  #download file if necessary
  dir.create("data", showWarnings = FALSE)
  if (!file.exists(dataClassificationFile) || !file.exists(dataSummaryFile)){
    download.file(url = dataUrl, destfile = zippedFile) #Date first downloaded: "Mon Sep 21 10:10:43 2015"
    unzip(zippedFile, exdir = 'data')
  }
  d <- list(class = readRDS(dataClassificationFile), summary = readRDS(dataSummaryFile))
}

if (!exists('d')){
  d <- getProjectData()
}

#plot 1 question:
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from 
#all sources for each of the years 1999, 2002, 2005, and 2008.

library("dplyr")

#aggregate emissions over years
totalEmissions  <- 
  d$summary %>%
  group_by(year) %>%
  summarise(aggregatedEmissions = sum(Emissions))

#open the png device
png(filename = "plot1.png", width = 480, height = 480, units = "px")

#create plot
with(totalEmissions, plot(year, aggregatedEmissions, type = 'l', ylab = "Total PM2.5 Emissions (tons)"))
title("PM2.5 Emissions in the United States, 1999 to 2008")

#close the png device
dev.off()

