## myPlot4 builds the plot3.png acording to assignment
## getMySource is used to download and unzip the source data base when necessary

## Usage: myPlot4()

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

## myPlot4() is the main function that builds the plot according to the assignment

myPlot4 <- function () {
	
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
	
	## "convert the Date and Time variables to Date/Time classes in R"* (quote from the asignment)
	SourceData <- cbind(strptime(paste(SourceData[[1]], SourceData[[2]]), format="%d/%m/%Y %H:%M:%S"), SourceData )
	
	names(SourceData) <- c("FullDate", "Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", 
						   "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
	
	## Make plot and save it to .png file with 480x480 pixels 
	## Plot 4 combines four different linear plots
	
	png(file = "plot4.png", width = 480, height = 480)
	
	par(mfrow = c(2, 2))
	
	with (SourceData, plot(FullDate, Global_active_power, 
			type="l", 
			xlab = "",
			ylab = "GLobal Active Power"))
					
	with (SourceData, plot(FullDate, Voltage, 
			type="l", 
			xlab = "datetime",
			ylab = "Voltage"))
	
	with (SourceData, {	
		plot(FullDate, Sub_metering_1, type="l", col="black", xlab = "", ylab = "Energy sub metering" )
		points(FullDate, Sub_metering_2, type="l", col="red" )
		points(FullDate, Sub_metering_3, type="l", col="blue")
		legend("topright", pch = "-", col = c("black", "red", "blue"), 
			legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
		})
	
	with (SourceData, plot(FullDate, Global_reactive_power, 
			type="l", 
			xlab = "datetime",
			ylab = "Global_reactive_power"))

	
	dev.off()
	
}												## end myPlot4



