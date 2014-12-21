# Q3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

library("ggplot2")

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# setwd("SET THIS TO YOUR WORKING DIRECTORY")
setwd("C:/Users/USER/Dropbox/Programming/Coursera/DataScience_JH_specialization/4_Exploratory_Data_Analysis/4_Exploratory_Data_Analysis_Project_2")

# file names and locations
fileurl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
fileSource <- "data/Source_Classification_Code.rds"
fileSummary <- "data/summarySCC_PM25.rds"

# the script checks to see if the files exist in a subfolder called "data" off of the current working directory,
# otherwise it will download the zip file and unzip it into a temp folder
if (file.exists(fileSource) & file.exists(fileSummary))
{
  sourceDF <- readRDS(fileSource, refhook = NULL)
  summaryDF <- readRDS(fileSummary, refhook = NULL)
} else {
  # create a temporary directory
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")
  # download into the placeholder file
  download.file(fileurl, tf)
  # The data has now been downloaded to a temporary file, and the full path is contained in tf.
  # The unzip() function can be used to query the contents
  
  # get the name of the first file in the zip archive
  fname1 = unzip(tf, list=TRUE)$Name[1]
  fname2 = unzip(tf, list=TRUE)$Name[2]
  # unzip the files to the temporary directory
  unzip(tf, files=fname1, exdir=td, overwrite=TRUE)
  unzip(tf, files=fname2, exdir=td, overwrite=TRUE)
  # fileurl1 and 2 is the full path to the extracted files
  fileurl1 = file.path(td, fname1)
  fileurl2 = file.path(td, fname2)  
  sourceDF <- readRDS(fileurl1, refhook = NULL)
  summaryDF <- readRDS(fileurl2, refhook = NULL)
}

# make the year into a factor
summaryDF$year <- as.factor(summaryDF$year)

# let's now subset for only Baltimore, fips = "24510"
summaryDF.baltimore <- summaryDF[summaryDF$fips == "24510", ]

# Let's make a smaller DF with only the data we need and clean up names
EmPerYr <- data.frame(summaryDF.baltimore$Emissions, summaryDF.baltimore$type, summaryDF.baltimore$year)

# Let's make the names easier to read
names(EmPerYr) <- c("Emissions", "Type", "Year")

# and let's sum up the Emissions per year
attach(EmPerYr) # yup, I used attach, one of the uglier R commands
#sumEmPerYr <- aggregate(EmPerYr$Emissions, by = list(EmPerYr$type, EmPerYr$year), FUN = sum)
sumEmPerYr <- aggregate(Emissions, by = list(Type, Year), FUN = sum)
# need to change the names
names(sumEmPerYr) <- c("Type", "Year", "Emissions")

# send plot to the png device...comment this out to view on screen
png(filename = "plot3.png", width = 580, height = 480, units = "px")

p <- ggplot(data = sumEmPerYr, aes(x = Year, y = Emissions))
p + geom_bar(stat="identity") + facet_grid(. ~ Type) + labs(title = "PM2.5 emissions for Baltimore") + geom_smooth(method="lm",se=FALSE, aes(group=3), col = "red")

# comment this out to view on screen
dev.off()
