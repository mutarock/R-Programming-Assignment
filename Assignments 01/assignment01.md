Programming Assignment 1: Air Pollution
=====

##Assignment Link
[Link](https://class.coursera.org/rprog-003/assignment/view?assignment_id=3)


##DataSet
[Link](https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip)

##Part 1
Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows

You can see some [example output from this function](https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fpollutantmean-demo.html). The function that you write should be able to match this output. Please save your code to a file named **pollutantmean.R**.


##Part 2
Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows

You can see some [example output from this function](https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcomplete-demo.html). The function that you write should be able to match this output. Please save your code to a file named complete.R. To run the submit script for this part, make sure your working directory has the file **complete.R** in it.


##Part 3
Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. Please read the help page for this function via '?cor' and make sure that you know how to use it.
You can see some [example output from this function](https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fcorr-demo.html). The function that you write should be able to match this output. Please save your code to a file named corr.R. To run the submit script for this part, make sure your working directory has the file **corr.R** in it.