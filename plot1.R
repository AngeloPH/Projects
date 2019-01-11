library(readr)
power <- subset(read_delim("household_power_consumption.txt", 
                           ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                        Time = col_time(format = "%H:%M:%S")), 
                           trim_ws = TRUE), Date == "2007-02-01"|Date == "2007-02-02")

par(mar = c(6,6,6,2))
with(power, hist(Global_active_power, main = "Global Active Power",
    xlab="Global Active Power (kilowatts)", ylim = c(0,1200),
    col = "red"))

dev.copy(png, file = "plot1.png")
dev.off()