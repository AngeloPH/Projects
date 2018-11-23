best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    ## data <- data[complete.cases(data),]
    ## Check that state and outcome are valid
    j = 0
    if (state %in% data$State) {} else stop("invalid state")
    if (outcome %in% c("heart attack","heart failure", "pneumonia")) {
        if (outcome == "heart attack") {
            j = 11
        } else if (outcome == "heart failure") {
            j = 17
        } else {
            j = 23
        }
    } else {stop("invalid outcome")}
    
    ## Return hospital name in that state with lowest 30-day death
    low = 0
    row = 0
    for (i in 1:nrow(data)) {
        if (data[i,7]==state) {
            if (low == 0) {
            low = data[i,j]
            row = i
            } else {
                if (low > data[i,j]) {
                    low = data[i,j]
                    row = i
                } else {
                    low = low
                    row = row
                }
            }
        }
    }
    data[row,2]
    ## rate
}