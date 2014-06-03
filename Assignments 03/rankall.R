rankall <- function(outcome, num = "best") {

        ## Read outcome data
        myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        myData <- myData[c(2, 7, 11, 17, 23)]
        myData[, c(3, 4, 5)] <- sapply(myData[, c(3, 4, 5)], as.numeric)
    
        ## Check that state and outcome are valid

        stateList <- unique(myData[, "State"])
        stateList <- sort(stateList)

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        ranks <- data.frame(hospital=NA, state=NA)
  
        for (i in 1:length(stateList)) {
                ranks[i, ] <- c(rankhospital(stateList[i], outcome, num), stateList[i])
        }
  
        ranks
}