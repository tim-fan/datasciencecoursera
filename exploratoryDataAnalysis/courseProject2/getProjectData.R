getProjectData <- function(){
  dataUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
  zippedFile <- 'data/exdata-data-NEI_data.zip'
  dataClassificationFile <- 'data/Source_Classification_Code.rds'
  dataSummaryFile <- 'data/summarySCC_PM25.rds'
  
  #download file if necesarry
  dir.create("data", showWarnings = FALSE)
  if (!file.exists(dataClassificationFile) || !file.exists(dataSummaryFile)){
    download.file(url = dataUrl, destfile = zippedFile) #
    unzip(zippedFile, exdir = 'data')
  }
  d <- list(class = readRDS(dataClassificationFile), summary = readRDS(dataSummaryFile))
}