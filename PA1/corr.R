corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        nobs_frame <- complete(directory)
        ids <- which(nobs_frame$nobs >= threshold)
        
        result <- numeric(0)
        
        for (id in ids) {
                data_frame <- getmonitor(id, directory)
                corelation <- cor(data_frame$sulfate, data_frame$nitrate, use = "na.or.complete")
                result <- c(result, corelation)
        }
        
        return(result)
}

