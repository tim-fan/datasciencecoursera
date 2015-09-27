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

#plot6 question:
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor
#vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen
#greater changes over time in motor vehicle emissions?

library("dplyr")
library("ggplot2")
library("gridExtra")

#same logic from plot 5 for choosing mobile vehicle SCC codes
mobileVehicleClasses <- 
  d$class %>% 
  filter(grepl('Mobile', EI.Sector) & !grepl('Equipment',EI.Sector))
mobileVehicleSccs <- unique(mobileVehicleClasses$SCC)

#In absolute terms the changes in Los Angeles County emissions are much greater
#than those for Baltimore. However in terms of percentage changes from 2009 levels,
#Baltimore city has seen greater changes.
#I will plot both absolute and relative levels, and let the reader decide which is more
#useful.

#helper functions
prcntOfFirstEl <- function(x) {x / first(x) * 100}
fipsToString <- function(fips) {if (fips == "06037") "Los Angeles County" else "Baltimore City"}

#filter for the two counties in question, aggregate emissions over years,
#add a column for percentage change since 2009, and add a column for
#county name
baltimoreLATotalEmissions <-  
  d$summary %>%
  filter(fips %in% c("24510", "06037"), SCC %in% mobileVehicleSccs) %>% 
  group_by(year, fips) %>% 
  summarise(aggregatedEmissions = sum(Emissions)) %>%
  group_by(fips) %>%
  mutate(aggregatedEmissionPrcntOfBaseYear = prcntOfFirstEl(aggregatedEmissions )) %>%
  rowwise() %>%
  mutate(County = fipsToString(fips))

#open the png device
png(filename = "plot6.png", width = 480, height = 480, units = "px")

#create plots
p1 <- qplot(
  x = year, 
  y = aggregatedEmissions, 
  data = baltimoreLATotalEmissions, 
  geom = 'line', 
  color = County,
  ylab = "Total Emissions\nPM2.5 in tons"
)

p2 <- qplot(
  x = year, 
  y = aggregatedEmissionPrcntOfBaseYear, 
  data = baltimoreLATotalEmissions, 
  geom = 'line', 
  color = County,
  ylim = c(0,160),
  ylab = "Total Emissions\nPercentage of 1999 levels"
)
grid.arrange(p1,p2, top = "PM2.5 Emissions from Motor Vehicle Sources\nin Baltimore City and LA County, 1999 to 2008")

#close the png device
dev.off()
