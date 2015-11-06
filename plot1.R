plot1 <- function(fileloc = "household_power_consumption.txt") {
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
        png("plot1.png")
        data <- createData()
        hist(data$Global_active_power, col= "red", 
             xlab = "Global Active Power (kilawatts)", main = "Global Active Power")
        dev.off()
    }
    plot_it()    
}