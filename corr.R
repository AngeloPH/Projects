corr <- function(directory, threshold = 0) {
    x <- vector()
    if (getwd() == directory) {
        library(readr)
        library(crayon)
        temp = getwd()
        y <- complete(directory, id = 1:332)
        high <- cbind(y$obs <= threshold,y$obs <= threshold)
        y <- y[!high]
        dim(y) <- c(((length(y))/2),2)
        colnames(y) <- c("id","obs")
        for (i in y[,1]) {
            if (nchar(i)==1) {
                z <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                    sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            } else if (nchar(i)==2) {
                z <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                   sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            } else {
                z <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                           sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            }
        }
    } else {
        library(readr)
        library(crayon)
        temp = getwd()
        y <- complete(directory, id = 1:332)
        setwd(directory)
        high <- cbind(y$obs <= threshold,y$obs <= threshold)
        y <- y[!high]
        dim(y) <- c(((length(y))/2),2)
        colnames(y) <- c("id","obs")
        for (i in y[,1]) {
            if (nchar(i)==1) {
                z <- read_csv("00" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                    sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            } else if (nchar(i)==2) {
                z <- read_csv("0" %+% as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                                   sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            } else {
                z <- read_csv(as.character(i) %+% ".csv", col_types = cols(nitrate = col_number(), 
                                                                           sulfate = col_number()))
                correlate <- cor(x = z[,2],y = z[,3], method = "pearson", use = "pairwise.complete.obs")
                x <- c(x,correlate)
            }
        }
    }
    bad <- is.na(x)
    setwd(temp)
    x[!bad]
}