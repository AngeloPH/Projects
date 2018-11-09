complete <- function(directory, id = 1:332) {
    y <- data.frame(id = numeric(length(id)),obs = numeric(length(id)))
    j = 1
    if (getwd() == directory) {
        library(readr)
        library(crayon)
        while (j <= length(id)) {
            for (i in id) {
                if (nchar(i)==1) {
                    x <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                        sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                } else if (nchar(i)==2) {
                    x <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                       sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                } else {
                    x <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                               sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                }
                j = j + 1
            }
        }
    } else {
        library(readr)
        library(crayon)
        temp = getwd()
        setwd(directory)
        while (j <= length(id)) {
            for (i in id) {
                if (nchar(i)==1) {
                    x <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                        sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                } else if (nchar(i)==2) {
                    x <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                       sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                } else {
                    x <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                               sulfate = col_number()))
                    y[j,1] <- i
                    y[j,2] <- sum(complete.cases(x[,1],x[,2]))
                }
                j = j + 1
            }
        }
    }
    setwd(temp)
    y
}