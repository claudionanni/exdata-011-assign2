### pm25_analyse.R : download, merge, clean, export  dataset from EPA NEI ### 


# We will use some fuctions, especially ddply to group and summarize.

#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("grid")
#install.packages("gridExtra")

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Download zip file if not done already
if (!file.exists('NEI_data.zip')) {
    download.file(paste0('https://d396qusza40orc.cloudfront.net/',
                         'exdata%2Fdata%2FNEI_data.zip'),
                  method='curl', destfile='NEI_data.zip')
    unzip('NEI_data.zip')
}

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



# NOTE: Although the dataset contains only PM2.5 pollutant data, I filter it, so this script can be used with datasets containing multiple pollutants.
NEI_pm25 <- NEI[NEI$Pollutant == "PM25-PRI",]



# > QUESTION 4 "Total emissions from PM2.5 in United States from 1999 to 2008 from Coal related sources" < #

# Getting the source IDs from the SDD data frame, only for coal combustion-related sources
coalRelatedSources <- SCC[grep("Coal", SCC$EI.Sector), 'SCC']

###########################################################
# > unique(SCC[SCC$SCC %in% coalRelatedSources, 4])
# [1] Fuel Comb - Electric Generation - Coal     
# [2] Fuel Comb - Industrial Boilers, ICEs - Coal
# [3] Fuel Comb - Comm/Institutional - Coal      
# 59 Levels: Agriculture - Crops & Livestock Dust ...
###########################################################
# Subset for only Coal related sources
NEI_pm25_coal <- NEI_pm25[NEI_pm25$SCC %in% coalRelatedSources,]

q4_data <- ddply(NEI_pm25_coal, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))

png(filename='plot4.png',width=640,height=640,units="px")

barplot(
  q4_data$TotalPerYear/10^3,
  names.arg=q4_data$year,
  xlab="Year",
  ylab="PM2.5 Emissions (Thousands of Tons)",
  main="USA PM2.5 Emission in years 1999,2002,2005,2008 from coal combustion sources"

)

#plot(q4_data,col="black",
#	xlab="Year",
#	ylab="PM25",
#	cex=2,
#	pch=18,
#	main="USA PM2.5 Emission in years 1999,2002,2005,2008 from coal combustion sources")
#smoothingSpline = smooth.spline(q4_data$year,q4_data$TotalPerYear, spar=0)
#lines(smoothingSpline,col="grey")

dev.off()





