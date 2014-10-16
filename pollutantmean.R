pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        file.names <- list.files(directory)
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        file.numbers <- as.numeric(sub('\\.csv$', '', file.names))
        
        selected.files = na.omit(file.names[match(id, file.numbers)])
        
        selected.dfs <- lapply(file.path(directory, selected.files), read.csv)
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        mean(c(sapply(selected.dfs, function(x) x[, pollutant])), na.rm=TRUE)
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
}