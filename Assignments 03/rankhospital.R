rankhospital <- function(state, outcome, num = "best") {

        ## Read outcome data
        myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        myData <- myData[c(2, 7, 11, 17, 23)]
        myData[, c(3, 4, 5)] <- sapply(myData[, c(3, 4, 5)], as.numeric)
        
        ## Check that state and outcome are valid
        stateList <- myData[myData$State == state, ]

        if(nrow(stateList) == 0) {
                stop("invalid state")
        }


        outcomes <- c("heart attack", "heart failure", "pneumonia")

        if (!(outcome %in% outcomes)) {
                stop("invalid outcome")
        }



        ## Return hospital name in that state with the given rank
        ## 30-day death rate

        if (outcome == "heart attack") {
                stateList <- stateList[order(stateList[, 3], stateList[, 1]), ]

                stateList <- stateList[!is.na(stateList[, 3]), ]
        
        } else if (outcome == "heart failure") {
                stateList <- stateList[order(stateList[, 4], stateList[, 1]), ]

                stateList <- stateList[!is.na(stateList[, 4]), ]    

        } else {
                stateList <- stateList[order(stateList[, 5], stateList[, 1]), ]

                stateList <- stateList[!is.na(stateList[, 5]), ]        

        }
  

        if (num == "best") {
                num <- 1L
        
        } else if (num == "worst") {
                num <- nrow(stateList)
  
        } else {
                num <- as.numeric(num)

        }


        stateList[num, 1]

}
