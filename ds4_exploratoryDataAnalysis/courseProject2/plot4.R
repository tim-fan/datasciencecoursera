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

#plot 4 question:
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library("dplyr")
library("ggplot2")

#find SCC codes related to coal combustion.
#To do this, just grep for rows where the word 'coal' AND 'combustion' appear.
#However, exclude columns 'Short.Name' and 'EI.Sector' from the grep, to avoid categories 
#like 'Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal', which appear to include coal
#as well as non-coal related sources (too general!)

#make functions for grepping for 'coal' and 'combustion'
elementsContainString <- function(string) {function(elements) {any(grepl(paste0('\\b',string,'\\b'),elements, ignore.case = TRUE))}}
containsCoal <- elementsContainString('coal')
containsCombustion <- elementsContainString('combustion')

#do the grep and extract the remaining SCC codes
coalCombustionClasses <- 
  d$class %>%
  select(-Short.Name, -EI.Sector) %>% #remove these columns from the following grep - they contain 'or' values
  filter(apply(., 1, containsCoal), apply(., 1, containsCombustion)) %>% 
  select(SCC)
coalCombustionSCCs <- coalCombustionClasses$SCC

#now we know which codes relate to coal combustion. Filter the summary data
#for coal-combustion related data, and aggregate over years
coalCombustionData <- 
  d$summary %>%
  filter(SCC %in% coalCombustionSCCs) %>%
  group_by(year) %>%
  summarise(aggregatedEmissions = sum(Emissions))

#now we can plot the trend for aggregate coal combustion related emissions

#open the png device
png(filename = "plot4.png", width = 480, height = 480, units = "px")

#create plot
qplot(
  x = year, 
  y = aggregatedEmissions, 
  data = coalCombustionData, 
  geom = 'line',
  ylab = "Total PM2.5 Emissions (tons)",
  main = "PM2.5 Emissions from Coal Combustion-Related Sources\nin the United States, 1999 to 2008"
)

#close the png device
dev.off()