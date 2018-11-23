rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    ## data <- data[complete.cases(data),]
    data <- data[order(data[,7], decreasing = FALSE, na.last = TRUE),]
    
    ## Check that state and outcome are valid
    j = 0
    #if (state %in% data$State) {} else stop("invalid state")
    if (outcome %in% c("heart attack","heart failure", "pneumonia")) {
        if (outcome == "heart attack") {
            j = 11
        } else if (outcome == "heart failure") {
            j = 17
        } else {
            j = 23
        }
    } else {stop("invalid outcome")}
    
    ## For each state, find the hospital of the given rank
    count = 0
    a = 1
    m = 1
    df <- data.frame("Hospital.Name" = rep(NA, nrow(data)), "Rate" = rep(NA, nrow(data)))
    final <- data.frame("Hospital" = rep(NA, 54), "State" = rep(NA, 54))
    for (m in 1:54) {
        for (i in 1:nrow(data)) {
            if (data[i,7]==data[a,7]) {
                df[i,1] <- data[i,2]
                df[i,2] <- data[i,j]
                count = count + 1
            }
        }
        df <- df[order(df[,1], decreasing = FALSE, na.last = TRUE),]
        df <- df[order(df[,2], decreasing = FALSE, na.last = TRUE),]
        df <- df[complete.cases(df),]
        if (num == "best") {
            final[m,1] <- df[1,1]
            final[m,2] <- data[a,7]
        } else if (num == "worst") {
            final[m,1] <- df[count,1]
            final[m,2] <- data[a,7]
        } else {
            final[m,1] <- df[num,1]
            final[m,2] <- data[a,7]
        }
        df <- data.frame("Hospital.Name" = rep(NA, nrow(data)), "Rate" = rep(NA, nrow(data)))
        m = m + 1
        a = a + count
        count = 0
    }
    final
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}