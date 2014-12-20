# Q4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# library("ggplot2", lib.loc="~/R/win-library/3.1")

setwd("C:/Users/werner/Dropbox/Programming/Coursera/DataScience_JH_specialization/4_Exploratory_Data_Analysis/4_Exploratory_Data_Analysis_Project_2")
fileSource <- "data/Source_Classification_Code.rds"
fileSummary <- "data/summarySCC_PM25.rds"

sourceDF <- readRDS(fileSource, refhook = NULL)
summaryDF <- readRDS(fileSummary, refhook = NULL)

# let's get the "Coal" entries in EI.Sector
coal_entries <- grep("[Cc]oal", sourceDF$EI.Sector) # 99 of these entries

# get the SCC entries based on the coal entries
sourceSCC <- as.character(sourceDF$SCC[coal_entries])
# just checking to see that they are strings
str(sourceSCC)

# ok, here is the magic..if you haven't checked out "which" and "%in%" you should...
# this returns the rows where summarySCC is related to coal in sourceDF
summaryCoalDF <- summaryDF[which(summaryDF$SCC %in% sourceSCC), ]

# make the year into a factor
summaryCoalDF$year <- as.factor(summaryCoalDF$year)

# Let's make a smaller DF with only the data we need and clean up names
EmPerYr <- data.frame(summaryCoalDF$Emissions, summaryCoalDF$year)
names(EmPerYr) <- c("Emissions", "year")
# and let's sum up the Emissions per year
sumEmPerYr <- aggregate(. ~ year, data=EmPerYr, FUN=sum)
# and let's scale this down since the values are in units of 10^5 (so divide by 10^5)
sumEmPerYr$Emissions <- mapply((function(x) x / 100000),  sumEmPerYr$Emissions)

# barchart
Emissions <- c(sumEmPerYr$Emissions)
bplt <- barplot(Emissions, ylim = c(0, 6), main="PM2.5 emissions from all Coal sources (US)", xlab="Year", ylab = "Emisisons (10^5 tons)", names.arg=c("1999", "2002", "2005", "2008"))
# for clarity's sake, let's add text to each bar showing the actual emissions amount
text(bplt, Emissions - 1, labels = round(Emissions, 2))
model <- lm(Emissions ~ year, sumEmPerYr)
# abline(model, lwd = 2, col = "red")


