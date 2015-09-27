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


#plot2 question:
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

library("dplyr")

#filter for Baltimore, and aggregate emissions over years
baltimoreTotalEmissions <-  
  d$summary %>%
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarise(aggregatedEmissions = sum(Emissions))

#open the png device
png(filename = "plot2.png", width = 480, height = 480, units = "px")

#create plot
with(baltimoreTotalEmissions, plot(year, aggregatedEmissions, type = 'l', ylab = "Total PM2.5 Emissions (tons)"))
title("PM2.5 Emissions in Baltimore City, 1999 to 2008")

#close the png device
dev.off()
