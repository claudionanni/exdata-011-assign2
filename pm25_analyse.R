### pm25_analyse.R : download, merge, clean, export  dataset from EPA NEI ### 


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

# We will use some fuctions, especialluy ddply to group and summarize.
library(plyr)

## > QUESTION 1 "Total emissions from PM2.5 in the United States from 1999 to 2008" < #

# Use ddply to group by "year" and sum "Emissions"
q1_data <- ddply(NEI_pm25, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))

png(filename='plot1.png',width=640,height=640,units="px")
plot(q1_data,col="red",
	xlab="Year",
	ylab="PM2.5",
	cex=1.5,
	pch=15,
	main="USA PM2.5 Emission in years 1999,2002,2005,2008.")

smoothingSpline = smooth.spline(q1_data$year,q1_data$TotalPerYear, spar=0)
lines(smoothingSpline,col="grey")

dev.off()


## > QUESTION 2 "Total emissions from PM2.5 in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008" < #

# Get only Baltimore observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]

# Use ddply to group by "year" and sum "Emissions"
q2_data <- ddply(NEI_pm25_baltimore, c("year"), summarise,
                 TotalPerYear    = sum(Emissions))

png(filename='plot2.png',width=640,height=640,units="px")
plot(q2_data,
	col="blue",
	xlab="Year",
	ylab="PM2.5",
	cex=1.5,
	pch=15,
	main="Baltimore PM2.5 Emission in years 1999,2002,2005,2008.")

smoothingSpline = smooth.spline(q2_data$year,q2_data$TotalPerYear, spar=0)
lines(smoothingSpline,col="grey")

dev.off()

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
    ylab = "Total Emissions per Year",
    main = "Baltimore PM2.5 Emission in years 1999,2002,2005,2008 per each source type")))
dev.off()

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
plot(q4_data,col="black",
	xlab="Year",
	ylab="PM25",
	cex=2,
	pch=18,
	main="USA PM2.5 Emission in years 1999,2002,2005,2008 from coal combustion sources")

smoothingSpline = smooth.spline(q4_data$year,q4_data$TotalPerYear, spar=0)
lines(smoothingSpline,col="grey")

dev.off()


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
plot(q5_data,col="orange",
	xlab="Year",
	ylab="PM25",
	cex=1.2,
	pch=16,
	main="Baltimore PM2.5 Emission in years 1999,2002,2005,2008 from motor vehicles sources")
smoothingSpline = smooth.spline(q5_data$year,q5_data$TotalPerYear, spar=0)
lines(smoothingSpline,col="grey")

dev.off()




# > QUESTION 6 "Total emissions from PM2.5 in Baltimore from 1999 to 2008 from motor Vehicles" < #
# Get only Baltimore observations
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


q6_data_1 <- ddply(NEI_pm25_baltimore_vehicle, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))
q6_data_1$City="Baltimore"

q6_data_2 <- ddply(NEI_pm25_losangeles_vehicle, c("year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))
q6_data_2$City="Los Angeles"

# Binding data to plot on same graph with ggplot2

q6_data <-rbind(q6_data_1,q6_data_2)

png(filename='plot6.png',width=640,height=640,units="px")
# Using smoothing with very little amount of points generates some warnings, we suppress them here.
suppressWarnings(print(qplot(year,
    TotalPerYear,
    data = q6_data,
    geom = c("smooth","point"),
    method="loess",
    colour = City,
    xlab = "Year",
    ylab = "Total Emissions per Year",
    main = "Baltimore vs Los Angeles PM2.5 Emission in years 1999,2002,2005,2008 from vehicles")))
dev.off()



