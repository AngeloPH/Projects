library(readr)
power <- subset(read_delim("household_power_consumption.txt", 
                           ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                        Time = col_time(format = "%H:%M:%S")), 
                           trim_ws = TRUE), Date == "2007-02-01"|Date == "2007-02-02")

power$DateTime <- as.POSIXct(paste(power$Date,power$Time))
par(mar = c(6,6,6,2))
with(power, plot(DateTime, Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", 
    xlab = ""))

dev.copy(png, file = "plot2.png")
dev.off()