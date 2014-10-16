complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        nobs_vector <- c()
        for(i in 1:332) {
                if(i<10) {
                        path <- paste(directory, "/00", id[id], ".csv", sep="")
                }
                if(i > 9 & i < 100) {
                        path <- paste(directory, "/0",id[i], ".csv", sep="")
                }
                if(i > 99 & i < 333) {
                        path <- paste(directory,"/",id[i],".csv",sep="")
                }
                mydata <- read.csv(path)
                #nobs <- nrow(na.omit(mydata))
                nobs <- sum(complete.cases(mydata))
                #Add the value to the end of the vector
                nobs_vector <- c(nobs_vector, nobs)
        }
        print(nobs_vector)
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
}