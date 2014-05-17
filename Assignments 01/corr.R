corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations


        totalComplete <- complete(directory, 1:332)

        nobsVec <- totalComplete["nobs"]

        resVector <- numeric()

        for (i in 1:332) {
                if (nobsVec[i, ] > threshold) {
                        fileNumber <- sprintf("%03d", i) ## trying to format the file number with leading zeros
                        fileName <- paste( c(directory,"/",fileNumber,".csv"), collapse="" ) ## file name
                        singleData <- read.csv(fileName)   

                        singleData <- singleData[complete.cases(singleData), ]


                        sulfate <- singleData[["sulfate"]]
                        nitrate <- singleData[["nitrate"]]

                        result <- cor(sulfate, nitrate)
                        resVector <- append(resVector, result)
                }

        }

        resVector
}