# Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

setwd("C:/Users/werner/Dropbox/Programming/Coursera/DataScience_JH_specialization/4_Exploratory_Data_Analysis/4_Exploratory_Data_Analysis_Project_2")
fileSource <- "data/Source_Classification_Code.rds"
fileSummary <- "data/summarySCC_PM25.rds"

sourceDF <- readRDS(fileSource, refhook = NULL)
summaryDF <- readRDS(fileSummary, refhook = NULL)

# make the year into a factor
summaryDF$year <- as.factor(summaryDF$year)

# Let's make a smaller DF with only the data we need and clean up names
EmPerYr <- data.frame(summaryDF$Emissions, summaryDF$year)
names(EmPerYr) <- c("Emissions", "year")
# and let's sum up the Emissions per year
sumEmPerYr <- aggregate(. ~ year, data=EmPerYr, FUN=sum)
# and let's scale this down since the values are in units of 10^6
sumEmPerYr$Emissions <- mapply((function(x) x / 1000000),  sumEmPerYr$Emissions)

# barchart
Emissions <- c(sumEmPerYr$Emissions)
bplt <- barplot(Emissions, ylim = c(0, 8), main="PM2.5 emissions from all sources (US)", xlab="Year", ylab = "Emisisons (million tons)", names.arg=c("1999", "2002", "2005", "2008"))
# for clarity's sake, let's add text to each bar showing the actual emissions amount
text(bplt, Emissions - 1, labels = round(Emissions, 2))
model <- lm(Emissions ~ year, sumEmPerYr)
# abline(model, lwd = 2, col = "red")