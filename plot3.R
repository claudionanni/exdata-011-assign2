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


# > QUESTION 3 "Total emissions from PM2.5 in Baltimore from 1999 to 2008 per source type (point, nonpoint, onroad, nonroad)" < #

# Get only Baltimore observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]

q3_data <- ddply(NEI_pm25_baltimore, c("type","year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))

png(filename='plot3.png',width=640,height=640,units="px")

# Using smoothing with very little amount of points generates some warnings, we suppress them here.
suppressWarnings(print(qplot(year,
    TotalPerYear,
    data = q3_data,
    geom = c("smooth","point"),
    method = "loess",
    colour = type,
    xlab = "Year",
    ylab = "Total Emissions per Year (Tons)",
    main = "Baltimore PM2.5 Emission in years 1999,2002,2005,2008 per each source type")))
dev.off()




