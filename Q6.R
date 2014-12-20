# Q6. Compare emissions from motor vehicle sources in Baltimore City (fips == "24510" with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

library(ggplot)

setwd("C:/Users/werner/Dropbox/Programming/Coursera/DataScience_JH_specialization/4_Exploratory_Data_Analysis/4_Exploratory_Data_Analysis_Project_2")
fileSource <- "data/Source_Classification_Code.rds"
fileSummary <- "data/summarySCC_PM25.rds"

sourceDF <- readRDS(fileSource, refhook = NULL)
summaryDF <- readRDS(fileSummary, refhook = NULL)

# let's look at how many unique entries there are in sourceDF@EI.Sector
# unique(sourceDF$SCC.Level.Two)
# ok, it looks like we need to get all the type of highway vehicles from sourceDF
highway_vehicles <- grep("Highway Vehicle", sourceDF$SCC.Level.Two)

# get the SCC.Level.Two entries based on the highway vehicles entries
sourceSCCL2 <- as.character(sourceDF$SCC[highway_vehicles])
# just checking to see that they are strings
str(sourceSCCL2)

# ok, here is the magic..if you haven't checked out "which" and "%in%" you should...
# this returns the rows where summarySCC is related to highway vehicles in sourceDF
summaryHwyVehDF <- summaryDF[which(summaryDF$SCC %in% sourceSCCL2), ]

# make the year into a factor
summaryHwyVehDF$year <- as.factor(summaryHwyVehDF$year)

# let's now subset for only Baltimore, fips = "24510" or LA, fips == "06037"
summaryHwyVehDF.bal.LA <- summaryHwyVehDF[summaryHwyVehDF$fips == "24510" | summaryHwyVehDF$fips == "06037", ]

# Let's make a smaller DF with only the data we need
EmPerYr <- data.frame(summaryHwyVehDF.bal.LA$Emissions, summaryHwyVehDF.bal.LA$year, summaryHwyVehDF.bal.LA$fips)

# and let's sum up the Emissions per year
attach(EmPerYr) # yup, I used attach, one of the uglier R commands
sumEmPerYr <- aggregate(Emissions, by = list(fips, year), FUN = sum)
# need to change the names
names(sumEmPerYr) <- c("City", "Year", "Emissions")

# Changing the City factor to city names
levels(sumEmPerYr$City)[1] <- "Los Angeles"
levels(sumEmPerYr$City)[2] <- "Baltimore"

p <- ggplot(data = sumEmPerYr, aes(x = Year, y = Emissions, fill = City))
p + geom_bar(stat="identity", position="dodge")  + xlab("Year") + ylab("Emissions (tons)") + labs(title = "PM2.5 Motor Vehicle Emissions for Los Angeles and Baltimore")