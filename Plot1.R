## myPlot1 builds the plot1.png acording to assignment
## getMySource is used to download and unzip the source data base when necessary

## Usage: myPlot1()

## getMySource() checks if the source file exists in working directory, 
## getMySource() downloads source archive and unzip it to working directory when necessary

getMySource <- function ( fileUrl, zipfilename, destfilename ) {
	if ( !file.exists( destfilename ) ) { 		## if destination file doesn't exist ...
		if ( !file.exists( zipfilename ) ) {	## if zipfilename doesn't exist download it
			download.file(fileUrl, zipfilename, method="curl")
			dateDownloaded <- Date()
		} 
		unzip(zipfilename, files = destfilename)
	} 											## if destfilename exists do nothing
} 												## end getMySource

## myPlot1() is the main function that builds the plot according to the assignment

myPlot1 <- function () {
	
	## get the source file
	
	fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
	zipfilename <- "household_power_consumption.zip"
	destfilename <- "household_power_consumption.txt"
	getMySource( fileUrl=fileUrl, zipfilename=zipfilename, destfilename=destfilename)	
	
	## load portion of the raw data
	## Since "[t]he dataset has 2,075,259 rows and 9 columns"* we "... will only be using [and reading] 
	## data from the dates 2007-02-01 and 2007-02-02"* *)quoted from the assignment

	startDate <- strptime("2007-02-01", format="%Y-%m-%d")
	endDate <- strptime("2007-02-02", format="%Y-%m-%d")
	na.strings <- "?"
	sep <- ";"
	
	## rows to skip were identified manually by
	## SourceData <- read.csv( destfilename )
	## SourceData$Date <- strptime(SourceData$Date, format="%d/%m/%Y") 
	## for (i in 1:nrow(SourceData)) { if ( SourceData$Date[i]>= startDate ) { print(i); stop()} }
	skip <- (66637-1)
	
	## rows to read were identified manually by 
	## SourceData <- read.csv( destfilename )
	## SourceData$Date <- strptime(SourceData$Date, format="%d/%m/%Y") 
	## for (i in 1:nrow(SourceData)) { if ( SourceData$Date[i]> endDate ) { print(i); stop()} }
	nrows <- (69517-1) - skip
	
	SourceData <- read.csv( destfilename, sep=sep, na.strings=na.strings, skip=skip, nrows=nrows )
	names(SourceData) <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
						   "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
	
	## "convert the Date and Time variables to Date/Time classes in R"* (quote from the asignment)
	## SourceData$Date <- strptime(SourceData$Date, format="%d/%m/%Y") ## not required for this plot
	
	## Make plot and save it to .png file with 480x480 pixels 
	## Plot 1 is a histogram with red bars
	
	png(file = "plot1.png", width = 480, height = 480)
	hist(SourceData$Global_active_power, col = "red", 
		main = "Global Active Power", 
		xlab = "GLobal Active Power (kilowatts)")
	dev.off()
	
}												## end myPlot1



