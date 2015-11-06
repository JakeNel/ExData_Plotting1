plot4 <- function(fileloc = "household_power_consumption.txt")  {
    ## Function createData() reads in the data, location specified by argument fileloc
    ## It then converts the Date column into a date format, and filters out data that
    ## does not match the dates of interest (February 1-2, 2007).  
    ## Lastly, it replaces Date and Time columns with a single column that stores the entire
    ## POSIXct value.  The function then returns the new version of the dataset
    createData <- function () {
        ## I load in my prefered packages data.table and dplyr 
        ## in order to use functions fread(), filter(), and select()
        library(data.table)
        library(dplyr)
        data <- fread(fileloc, na.strings = "?")
        data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
        data <- filter(data, Date == "2007-02-01" | Date == "2007-02-02")
        Date_Time <- paste(data$Date, data$Time)
        Date_Time <- as.POSIXct(strptime(Date_Time, format = "%Y-%m-%d %H:%M:%S"))
        data <- cbind(Date_Time, data)
        data <- select(data, -Date, -Time)
        return(data)
  }
    ## The plot_it() function opens a png device, creates the data using createData() 
    ## and creates 4 plots with par(c(2,2))
    plot_it <- function () {
        png("plot4.png")
        data <- createData()
        par(mfrow = c(2,2))
        plot(data$Date_Time, data$Global_active_power, type = "l", 
             xlab = "", ylab = "Global Active Power")
        plot(data$Date_Time, data$Voltage, type = "l", 
             xlab = "datatime", ylab = "Voltage")
        plot(data$Date_Time, data$Sub_metering_1, type = "l", 
             xlab = "", ylab = "Engergy sub metering")
        lines(data$Date_Time, data$Sub_metering_2, type = "l", col = "red")
        lines(data$Date_Time, data$Sub_metering_3, type = "l", col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               lty = 1, col = c("black", "red", "blue"), bty = "n")
        plot(data$Date_Time, data$Global_reactive_power, type = "l", 
             xlab = "datetime", ylab = "Global_reactive_power")
        dev.off()
    }
    plot_it()
}