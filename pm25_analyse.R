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
plot(q1_data,col="red",xlab="year",ylab="PM25",cex=1,pch=15,main="United States PM2.5 Emission in years 1999,2002,2005,2008.")


## > QUESTION 2 "Total emissions from PM2.5 in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008" < #

# Get only Baltimore observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]

# Use ddply to group by "year" and sum "Emissions"
q2_data <- ddply(NEI_pm25_baltimore, c("year"), summarise,
                 TotalPerYear    = sum(Emissions))
plot(q2_data,col="blue",xlab="year",ylab="PM25",cex=1,pch=15,main="Baltimore PM2.5 Emission in years 1999,2002,2005,2008.")


# > QUESTION 3 "Total emissions from PM2.5 in Baltimore from 1999 to 2008 per source type (point, nonpoint, onroad, nonroad)" < #

# Get only Baltimore observations
NEI_pm25_baltimore <- NEI_pm25[NEI_pm25$fips == "24510",]

q3_data <- ddply(NEI_pm25_baltimore, c("type","year"), summarise,
                 TotalPerYear    = sum(as.numeric(Emissions)))
qplot(year, TotalPerYear, data = q3_data, geom = "line",
    colour = type,
    xlab = "Year",
    ylab = "Total Emissions per Year",
    main = "Baltimore PM2.5 Emission in years 1999,2002,2005,2008 per each source type")

