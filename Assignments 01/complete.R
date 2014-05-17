complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases


        resultData <- data.frame(id=numeric(0), nobs=numeric(0))
        for( i in id )  { # iterating over the id s provided
                fileNumber <- sprintf("%03d", i) ##  format the file number with leading zeros
                fileName <- paste( c(directory,"/",fileNumber,".csv"), collapse="" ) ## file name
                singleData <- read.csv(fileName)    
                rowCount <- nrow(singleData[complete.cases(singleData), ])

                resultData <- rbind(resultData, data.frame(id=i, nobs=rowCount))
        }
        
        resultData
}