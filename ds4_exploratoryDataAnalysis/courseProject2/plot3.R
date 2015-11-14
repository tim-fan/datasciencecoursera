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

#plot3 question:
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
#a plot answer this question.

library("dplyr")
library("ggplot2")

#helper function: converts vector of values
#to vector of percentages of first value
prcntOfFirstEl <- function(x) {x / first(x) * 100}

#filter for Baltimore, aggregate emissions over years for each type,
#and convert absolute emissions to percentage of 2009 levels
aggregatesByType <- 
  d$summary %>%
  group_by(year, type) %>% 
  summarise(aggregatedEmissions = sum(Emissions)) %>%
  group_by(type) %>% 
  mutate(aggregatedEmissionPercentage = prcntOfFirstEl(aggregatedEmissions))

#open the png device
png(filename = "plot3.png", width = 480, height = 480, units = "px")

#create plot
qplot(
  x = year, 
  y = aggregatedEmissionPercentage, 
  data = aggregatesByType, 
  color = type, 
  geom = "line",
  ylim = c(0,120),
  ylab = "Total PM2.5 Emissions\nPercentage of 1999 levels",
  main = "PM2.5 Emissions by Type in Baltimore City, 1999 to 2008"
)

#close the png device
dev.off()