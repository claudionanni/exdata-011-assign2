### plot6.R : download, merge, clean, dataset from EPA NEI and make plot6.png ### 

# We will use some libraries, especially ddply to group and summarize and ggplot2 to graph.

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


# > QUESTION 6 "Which city among Baltimore and Los Angeles has seen greater changes over time in motor vehicle emissions?" < #

# Get only Baltimore and Los Angeles observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]
NEI_pm25_losangeles <- NEI_pm25[NEI_pm25$fips == "06037",]

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
NEI_pm25_losangeles_vehicle <- NEI_pm25_losangeles[NEI_pm25_losangeles$SCC %in% motorVehicle,]


# Creating two sub datasets that we will merge later

# Calculating Baltimore aggregate values
q6_data_1 <- ddply(NEI_pm25_baltimore_vehicle, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))
q6_data_1$City="Baltimore"


# Calculating Los Angeles aggregate values
q6_data_2 <- ddply(NEI_pm25_losangeles_vehicle, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))
q6_data_2$City="Los Angeles"


# Binding data to plot on same graph with ggplot2
# q6_data <-rbind(q6_data_1,q6_data_2)
# This graph has both plots on same canvas, but you can't appreciate the rate of change for each city since their scale is too different, Los Angeles is 10 times higher than Baltimore 
#suppressWarnings(print(
# qplot(year,
#    TotalPerYear,
#    data = q6_data,
#    geom = c("smooth","point"),
#    method="loess",
#    colour = City,
#    xlab = "Year",
#    ylab = "Total Emissions per Year",
#    main = "Baltimore vs Los Angeles PM2.5 Emission in years 1999,2002,2005,2008 from vehicles")
#))

#Being the question: "Which city has seen greater changes over time in motor vehicle emissions?"
#
# We want to consider the change(or relative change) within the city, so this kind of graph that normalizes the graph give a better idea of the change.

# So I went for a side by side plot on different facets so that the scale is different, and the graph is normalized to same size so that you can appreciate the rate of change for each city
# Plus I added regression line plots to have the idea of the general trend over the 10 years period, I find it helpful.

plot_baltimore <- qplot(year,
    TotalPerYear,
    data = q6_data_1,
    geom = c("smooth","point"),
    method="loess",
    xlab = "Year",
    ylab = "Total Emissions per Year (Tons)",
    main = "Baltimore")

plot_losangeles <- qplot(year,
    TotalPerYear,
    data = q6_data_2,
    geom = c("smooth","point"),
    method="loess",
    xlab = "Year",
    ylab = "Total Emissions per Year (Tons)",
    main = "Los Angeles")

plot_baltimore_lm <- qplot(year,
    TotalPerYear,
    data = q6_data_1,
    geom = c("smooth","point"),
    geom_smooth(method='lm'),
    method="lm",
    xlab = "Year",
    ylab = "Total Emissions per Year (Tons)",
    main = "Baltimore - Trend")

plot_losangeles_lm <- qplot(year,
    TotalPerYear,
    data = q6_data_2,
    geom = c("smooth","point"),
    method="lm",
    xlab = "Year",
    ylab = "Total Emissions per Year (Tons)",
    main = "Los Angeles  - Trend")


# Using smoothing with very little amount of points generates some warnings, we suppress them.

png(filename='plot6.png',width=640,height=640,units="px")

suppressWarnings(grid.arrange(plot_baltimore, plot_losangeles, plot_baltimore_lm, plot_losangeles_lm,  ncol = 2, nrow = 2, main = "PM2.5 Emission in years 1999,2002,2005,2008 from vehicles"))

dev.off()




