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

# Plot 5 question:
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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
  fill = EI.Sector,
  ylab = "Total PM2.5 Emissions (tons)",
  main = "PM2.5 Emissions from Motor Vehicle Sources\nin Baltimore City, 1999 to 2008"
)

#close the png device
dev.off()