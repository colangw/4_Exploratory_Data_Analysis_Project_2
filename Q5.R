# Q5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

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

# send plot to the png device...comment this out to view on screen
png(filename = "plot5.png", width = 480, height = 480, units = "px")

# barchart
Emissions <- c(sumEmPerYr$Emissions)
bplt <- barplot(Emissions, ylim = c(0, 4), main="PM2.5 emissions from Motor Vehicles (Baltimore)", xlab="Year", ylab = "Emisisons (10^2 tons)", names.arg=c("1999", "2002", "2005", "2008"))
# for clarity's sake, let's add text to each bar showing the actual emissions amount
text(bplt, Emissions - 0.5, labels = round(Emissions, 2))
model <- lm(Emissions ~ year, sumEmPerYr)
# abline(model, lwd = 2, col = "red")

# comment this out to view on screen
dev.off()