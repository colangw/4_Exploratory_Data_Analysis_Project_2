# Q5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

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

# let's now subset for only Baltimore, fips = "24510"
summaryHwyVehDF.baltimore <- summaryHwyVehDF[summaryHwyVehDF$fips == "24510", ]

# Let's make a smaller DF with only the data we need and clean up names
EmPerYr <- data.frame(summaryHwyVehDF.baltimore$Emissions, summaryHwyVehDF.baltimore$year)
names(EmPerYr) <- c("Emissions", "year")
# and let's sum up the Emissions per year
sumEmPerYr <- aggregate(. ~ year, data=EmPerYr, FUN=sum)
# and let's scale this down since the values are in units of 10^2 (so divide by 10^2)
sumEmPerYr$Emissions <- mapply((function(x) x / 100),  sumEmPerYr$Emissions)

# barchart
Emissions <- c(sumEmPerYr$Emissions)
bplt <- barplot(Emissions, ylim = c(0, 4), main="PM2.5 emissions from Motor Vehicles (Baltimore)", xlab="Year", ylab = "Emisisons (10^2 tons)", names.arg=c("1999", "2002", "2005", "2008"))
# for clarity's sake, let's add text to each bar showing the actual emissions amount
text(bplt, Emissions - 0.5, labels = round(Emissions, 2))
model <- lm(Emissions ~ year, sumEmPerYr)
# abline(model, lwd = 2, col = "red")
