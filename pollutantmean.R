pollutantmean <- function(directory, pollutant, id=1:332) {
    if (getwd() == directory) {
        library(readr)
        library(crayon)
        sum = 0
        count = 0
        for (i in id) {
            if (nchar(i)==1) {
                x <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                    sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            } else if (nchar(i)==2) {
                x <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                   sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            } else {
                x <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                           sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            }
        }
        mean <- sum/count
        print(mean)
    } else {
        library(readr)
        library(crayon)
        sum = 0
        count = 0
        temp = getwd()
        setwd(directory)
        for (i in id) {
            if (nchar(i)==1) {
                x <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                    sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            } else if (nchar(i)==2) {
                x <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                   sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            } else {
                x <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                           sulfate = col_number()))
                z <- match(pollutant,names(x))
                sum <- sum + sum(x[,z], na.rm = TRUE)
                y <- is.na(x[,z])
                count <- count + sum(!y)
            }
        }
        mean <- sum/count
        print(mean)
        setwd(temp)
   }
}