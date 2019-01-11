library(readr)
power <- subset(read_delim("household_power_consumption.txt", 
                           ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                        Time = col_time(format = "%H:%M:%S")), 
                           trim_ws = TRUE), Date == "2007-02-01"|Date == "2007-02-02")

power$DateTime <- as.POSIXct(paste(power$Date,power$Time))

par(mfcol = c(2,2), cex = 0.5)

with(power, plot(DateTime, Global_active_power, type = "l", ylab = "Global Active Power", 
                 xlab = ""))

plot(power$DateTime, power$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(power$DateTime, power$Sub_metering_2, type = "l", col = "red")
lines(power$DateTime, power$Sub_metering_3, type = "l", col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
      lty = 1, col = c("black", "red", "blue"), y.intersp = 0.25)

plot(power$DateTime, power$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")

plot(power$DateTime, power$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")

dev.copy(png, "plot4.png")
dev.off()