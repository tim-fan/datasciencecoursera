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

library("ggplot2")
library("dplyr")
# 
# #plot 1
# totalEmissions  <- 
#   d$summary %>%
#   group_by(year) %>%
#   summarise(aggregatedEmissions = sum(Emissions))
# 
# with(totalEmissions, plot(year, aggregatedEmissions, type = 'l'))
# 
# #plot2
# baltimoreTotalEmissions <-  
#   d$summary %>%
#   filter(fips == "24510") %>% 
#   group_by(year) %>% 
#   summarise(aggregatedEmissions = sum(Emissions))
# 
# with(baltimoreTotalEmissions, plot(year, aggregatedEmissions, type = 'l'))
# 
# #plot3
# prcntOfFirstEl <- function(x) {x / first(x) * 100}
# 
# aggregatesByType <- 
#   d$summary %>%
#   group_by(year, type) %>% 
#   summarise(aggregatedEmissions = sum(Emissions)) %>%
#   group_by(type) %>% 
#   mutate(aggregatedEmissionPercentage = prcntOfFirstEl(aggregatedEmissions))
# 
# qplot(year, aggregatedEmissionPercentage, data = aggregatesByType, color = type, geom = "line")
# 
# 
# #plot 4
# 
# #find SCC codes related to coal combustion.
# #To do this, just grep for rows where the word 'coal' AND 'combustion' appear.
# #However, exclude columns 'Short.Name' and 'EI.Sector' from the grep, to avoid categories 
# #like 'Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal', which appear to include coal
# #as well as non-coal related sources (too general!)
# 
# #make functions for grepping for 'coal' and 'combustion'
# elementsContainString <- function(string) {function(elements) {any(grepl(paste0('\\b',string,'\\b'),elements, ignore.case = TRUE))}}
# containsCoal <- elementsContainString('coal')
# containsCombustion <- elementsContainString('combustion')
# 
# #do the grep and extract the remaining SCC codes
# coalCombustionClasses <- 
#   d$class %>%
#   select(-Short.Name, -EI.Sector) %>% #remove these columns from the following grep - they contain 'or' values
#   filter(apply(., 1, containsCoal), apply(., 1, containsCombustion)) %>% 
#   select(SCC)
# coalCombustionSCCs <- coalCombustionClasses$SCC
# 
# #now we know which codes relate to coal combustion. Filter the summary data
# #for coal-commbustion related data
# coalCombustionData <- 
#   d$summary %>%
#   filter(SCC %in% coalCombustionSCCs) %>%
#   group_by(year) %>%
#   summarise(aggregatedEmissions = sum(Emissions))
# 
# #now we can plot the trend for aggregate coal combustion related emissions
# qplot(year, aggregatedEmissions, data = coalCombustionData, geom = 'line')
# 


# Plot 5:

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

qplot(
        x = year, 
        y = aggregatedEmissions, 
        data = baltimoreMobVehicleTotalEmissions, 
        geom = 'area', 
        fill = EI.Sector
      )

#plot6:
library(gridExtra)

#same logic from plot 5 for choosing mobile vehicle SCC codes
mobileVehicleClasses <- 
  d$class %>% 
  filter(grepl('Mobile', EI.Sector) & !grepl('Equipment',EI.Sector))
mobileVehicleSccs <- unique(mobileVehicleClasses$SCC)

#In absolute terms the changes in Los Angeles County emissions are much greater
#than those for Baltimore. 
prcntOfFirstEl <- function(x) {x / first(x) * 100}
fipsToString <- function(fips) {if (fips == "06037") "Los Angeles County" else "Baltimore City"}
baltimoreLATotalEmissions <-  
  d$summary %>%
  filter(fips %in% c("24510", "06037"), SCC %in% mobileVehicleSccs) %>% 
  group_by(year, fips) %>% 
  summarise(aggregatedEmissions = sum(Emissions)) %>%
  group_by(fips) %>%
  mutate(aggregatedEmissionPrcntOfBaseYear = prcntOfFirstEl(aggregatedEmissions )) %>%
  rowwise() %>%
  mutate(County = fipsToString(fips))

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