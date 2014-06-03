best <- function(state, outcome) {
    
        ## Read outcome data
        myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        ## Check that state and outcome are valid
        #stateList <- unique(myData[, "State"] == state)
        stateList <- myData[myData$State == state, ]
        #isStateExist <- sum(myData[, "State"] == state)

        if(nrow(stateList) == 0) {
                stop("invalid state")
        }


        outcomes <- c("heart attack", "heart failure", "pneumonia")

        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }


        ## Return hospital name in that state with lowest 30-day death
        ## rate

        stateList[, c(11, 17, 23)] <- sapply(stateList[, c(11, 17, 23)], as.numeric)
        stateList <- stateList[order(stateList[, 2]), ]

        result <- NULL

        if (outcome == "heart attack") {
                result <- stateList[which.min(stateList[, 11]), "Hospital.Name"]
        
        } else if (outcome == "heart failure") {
                result <- stateList[which.min(stateList[, 17]), "Hospital.Name"]
        
        } else {
                result <- stateList[which.min(stateList[, 23]), "Hospital.Name"]
        }
  
        result

}