plot3 <- function(fileloc = "household_power_consumption.txt")  {
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
    ## and creates the plot
    plot_it <- function () {
        png("plot3.png")
        data <- createData()
        plot(data$Date_Time, data$Sub_metering_1, type = "l", 
             xlab = "", ylab = "Engergy sub metering")
        lines(data$Date_Time, data$Sub_metering_2, type = "l", col = "red")
        lines(data$Date_Time, data$Sub_metering_3, type = "l", col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
             lty = 1, col = c("black", "red", "blue"))
        dev.off()
    }
    plot_it()
}