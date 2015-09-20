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

#plot 4
# coalCombustionSCCs <- 
#   d$class %>%
#   filter(grepl('combustion', SCC.Level.Four, ignore.case = TRUE) &&
#          grepl('coal',       SCC.Level.Four, ignore.case = TRUE)) %>%
#   select(SCC)

elementsContainString <- function(string) {function(elements) {any(grepl(paste0('\\b',string,'\\b'),elements, ignore.case = TRUE))}}
containsCoal <- elementsContainString('coal')
containsCombustion <- elementsContainString('combustion')
coalCombustionSCCs <- 
  d$class %>%
  select(-Short.Name, -EI.Sector) %>% #remove these columns from the following grep - they contain 'or' values
  filter(apply(., 1, containsCoal), apply(., 1, containsCombustion)) %>% 
  select(SCC) 

