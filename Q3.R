# Q3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

library("ggplot2", lib.loc="~/R/win-library/3.1")

setwd("C:/Users/werner/Dropbox/Programming/Coursera/DataScience_JH_specialization/4_Exploratory_Data_Analysis/4_Exploratory_Data_Analysis_Project_2")
fileSource <- "data/Source_Classification_Code.rds"
fileSummary <- "data/summarySCC_PM25.rds"

sourceDF <- readRDS(fileSource, refhook = NULL)
summaryDF <- readRDS(fileSummary, refhook = NULL)

# make the year into a factor
summaryDF$year <- as.factor(summaryDF$year)

# let's now subset for only Baltimore, fips = "24510"
summaryDF.baltimore <- summaryDF[summaryDF$fips == "24510", ]

# Let's make a smaller DF with only the data we need and clean up names
EmPerYr <- data.frame(summaryDF.baltimore$Emissions, summaryDF$type, summaryDF.baltimore$year)


# and let's sum up the Emissions per year
attach(EmPerYr) # yup, I used attach, one of the uglier R commands
sumEmPerYr <- aggregate(Emissions, by = list(type, year), FUN = sum)
# need to change the names
names(sumEmPerYr) <- c("Type", "Year", "Emissions")

p <- ggplot(data = sumEmPerYr, aes(x = Year, y = Emissions))
p + geom_bar(stat="identity") + facet_grid(. ~ Type) + labs(title = "PM2.5 emissions for Baltimore")

