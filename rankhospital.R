rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  count = 0
  df <- data.frame("Hospital.Name" = rep(NA, nrow(data)), "Rate" = rep(NA, nrow(data)))
  for (i in 1:nrow(data)) {
      if (data[i,7]==state) {
          df[i,1] <- data[i,2]
          df[i,2] <- data[i,j]
          count = count + 1
          }
  }
  df <- df[order(df[,1], decreasing = FALSE, na.last = TRUE),]
  df <- df[order(df[,2], decreasing = FALSE, na.last = TRUE),]
  df <- df[complete.cases(df),]
  if (num == "best") {
      df[1,1]
  } else if (num == "worst") {
      df[count,1]
  } else {
      df[num,1]
  }
  ## 30-day death rate
}