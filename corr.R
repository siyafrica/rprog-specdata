corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        all_records <- complete(directory)
        
        # subset(all_records,all_records[["nobs"]] > threshold)
        
        ids_above_threshold <- all_records[["id"]][all_records[["nobs"]] > threshold]
        
        cor_of_id <- function(id) cor(getmonitor(id,directory)[["nitrate"]],getmonitor(id,directory)[["sulfate"]],use="complete.obs")
        
        mapply(cor_of_id,ids_above_threshold)
}