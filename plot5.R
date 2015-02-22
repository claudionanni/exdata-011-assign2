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



# > QUESTION 5 "Total emissions from PM2.5 in Baltimore from 1999 to 2008 from motor Vehicles" < #
# Get only Baltimore observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]

# Getting the source IDs from the SDD data frame, only for vehicle combustion-related sources
motorVehicle <- SCC[grep("Vehicle", SCC$EI.Sector), 'SCC']

###########################################################
# > unique(SCC[SCC$SCC %in% motorVehicle, 4])
# [1] Mobile - On-Road Gasoline Light Duty Vehicles
# [2] Mobile - On-Road Gasoline Heavy Duty Vehicles
# [3] Mobile - On-Road Diesel Light Duty Vehicles  
# [4] Mobile - On-Road Diesel Heavy Duty Vehicles  
# 59 Levels: Agriculture - Crops & Livestock Dust ...
###########################################################

# Subset for only Vehicle related sources
NEI_pm25_baltimore_vehicle <- NEI_pm25_baltimore[NEI_pm25_baltimore$SCC %in% motorVehicle,]

q5_data <- ddply(NEI_pm25_baltimore_vehicle, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))

png(filename='plot5.png',width=640,height=640,units="px")

barplot(
  q5_data$TotalPerYear,
  names.arg=q5_data$year,
  xlab="Year",
  ylab="PM2.5 Emissions (Tons)",
  main="Baltimore PM2.5 Emission in years 1999,2002,2005,2008 from motor vehicles sources"

)

#plot(q5_data,col="orange",
#	xlab="Year",
#	ylab="PM25",
#	cex=1.2,
#	pch=16,
#	main="Baltimore PM2.5 Emission in years 1999,2002,2005,2008 from motor vehicles sources")
#smoothingSpline = smooth.spline(q5_data$year,q5_data$TotalPerYear, spar=0)
#lines(smoothingSpline,col="grey")

dev.off()




