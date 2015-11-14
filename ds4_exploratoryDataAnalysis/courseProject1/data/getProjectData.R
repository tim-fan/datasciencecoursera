getProjectData <- function(){
  #load data for project 1 of Exploratory Data Analysis
  datafileName <- "household_power_consumption.txt"
  
  #download file if neccessary
  if (!file.exists(datafileName)){
    dataUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    download.file(url = dataUrl, destfile = 'household_power_consumption.zip') #first downloaded "Wed Sep  9 18:24:53 2015"
    unzip('household_power_consumption.zip')
  }
  
  data <- read.csv2(datafileName, na.strings = '?', dec=".", colClasses = c(rep('character',2), rep('numeric',7)))
  
  #we are only interested in data from days 2007-02-01 and 2007-02-02
  data <- subset(data, grepl("^[12]/2/2007", Date))
  
  #convert date/time to POSIXct
  data <- within(data, {timestamp=strptime(paste(Date,Time),format = "%e/%m/%Y %H:%M:%S")})
}