getProjectData <- function(){
  dataUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
  zippedFile <- 'data/exdata-data-NEI_data.zip'
  dataClassificationFile <- 'data/Source_Classification_Code.rds'
  dataSummaryFile <- 'data/summarySCC_PM25.rds'
  
  #download file if necessary
  dir.create("data", showWarnings = FALSE)
  if (!file.exists(dataClassificationFile) || !file.exists(dataSummaryFile)){
    download.file(url = dataUrl, destfile = zippedFile) #
    unzip(zippedFile, exdir = 'data')
  }
  d <- list(class = readRDS(dataClassificationFile), summary = readRDS(dataSummaryFile))
}

if (!exists('d')){
  d <- getProjectData()
}




#plot 1
library("dplyr")

#aggregate emissions over years
totalEmissions  <- 
  d$summary %>%
  group_by(year) %>%
  summarise(aggregatedEmissions = sum(Emissions))

#open the png device
png(filename = "plot1.png", width = 480, height = 480, units = "px")

#create plot
with(totalEmissions, plot(year, aggregatedEmissions, type = 'l'))

#close the png device
dev.off()


#plot2
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
with(baltimoreTotalEmissions, plot(year, aggregatedEmissions, type = 'l'))

#close the png device
dev.off()




#plot3
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
        ylab = "Aggrregated Emissions\nPercentage of 1999 levels"
      )

#close the png device
dev.off()


#plot 4
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
        geom = 'line'
      )

#close the png device
dev.off()



# Plot 5:
library("dplyr")
library("ggplot2")

# Notes on determining 'motor vehicle' SCC codes:
#
# See below for EI.Sector values mobile sources:
# levels(d$class$EI.Sector)
# [43] "Mobile - Aircraft"                                 
# [44] "Mobile - Commercial Marine Vessels"                
# [45] "Mobile - Locomotives"                              
# [46] "Mobile - Non-Road Equipment - Diesel"              
# [47] "Mobile - Non-Road Equipment - Gasoline"            
# [48] "Mobile - Non-Road Equipment - Other"               
# [49] "Mobile - On-Road Diesel Heavy Duty Vehicles"       
# [50] "Mobile - On-Road Diesel Light Duty Vehicles"       
# [51] "Mobile - On-Road Gasoline Heavy Duty Vehicles"     
# [52] "Mobile - On-Road Gasoline Light Duty Vehicles"
#
# From these, I decided to exclude all 'equipment' as a type of vehicle,
# and to include trains, planes and boats as motorised vehicles (beacuse these are all types of vehicles with a motor)
# Note that the effects of including/excluding planes, trains and equipment is pretty small,
# but the effect of including boats is significant. I've color coded my plot to make the 
# contributions from various sources clear.

mobileVehicleClasses <- 
    d$class %>% 
    filter(grepl('Mobile', EI.Sector) & !grepl('Equipment',EI.Sector))

# Included EI.Sectors:
# > unique(mobileVehicleClasses$EI.Sector)
# [1] Mobile - On-Road Gasoline Light Duty Vehicles
# [2] Mobile - On-Road Gasoline Heavy Duty Vehicles
# [3] Mobile - On-Road Diesel Light Duty Vehicles  
# [4] Mobile - On-Road Diesel Heavy Duty Vehicles  
# [5] Mobile - Aircraft                            
# [6] Mobile - Commercial Marine Vessels           
# [7] Mobile - Locomotives           

mobileVehicleSccs <- unique(mobileVehicleClasses$SCC)

#now we have the relevant SCCs, filter the data to get 
#Baltimore motor-vehicle emissions by year

baltimoreMobVehicleTotalEmissions <- 
  d$summary %>%
  filter(fips == "24510", SCC %in% mobileVehicleSccs) %>% 
  left_join(d$class) %>%    #join to get EI.Sector codes, for color coding
  group_by(year, EI.Sector) %>% 
  summarise(aggregatedEmissions = sum(Emissions))

#open the png device
png(filename = "plot5.png", width = 700, height = 480, units = "px")

#create plot
qplot(
        x = year, 
        y = aggregatedEmissions, 
        data = baltimoreMobVehicleTotalEmissions, 
        geom = 'area', 
        fill = EI.Sector
      )

#close the png device
dev.off()



#plot6:
library("dplyr")
library("ggplot2")
library(gridExtra)

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
  ylab = "Aggrregated Emissions\nPM2.5 in tons"
)

p2 <- qplot(
  x = year, 
  y = aggregatedEmissionPrcntOfBaseYear, 
  data = baltimoreLATotalEmissions, 
  geom = 'line', 
  color = County,
  ylim = c(0,160),
  ylab = "Aggrregated Emissions\nPercentage of 1999 levels"
)
grid.arrange(p1,p2)

#close the png device
dev.off()
