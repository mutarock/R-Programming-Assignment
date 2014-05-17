pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

        totalData <- data.frame()
        for( i in id )  { # iterating over the id s provided
                fileNumber <- sprintf("%03d", i) ## trying to format the file number with leading zeros
                fileName <- paste( c(directory,"/",fileNumber,".csv"), collapse="" ) ## file name
                singleData <- read.csv(fileName)    

                tempData <- subset(singleData)    
                totalData <- rbind(totalData, tempData)
       }

       #totalData

       round(mean(totalData[,pollutant], na.rm = TRUE), 3)
}