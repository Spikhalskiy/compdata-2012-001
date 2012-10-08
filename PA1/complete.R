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
        
        nobs_vector = numeric(0)
        
        for (concrete_id in id) {
                frame <- getmonitor(concrete_id, directory)
                sulfate_defined <- !is.na(frame$sulfate)
                nitrate_defined <- !is.na(frame$nitrate)
                defined <- sulfate_defined & nitrate_defined
                nobs_vector <- c(nobs_vector, sum(defined))
        }
        
        result = data.frame(id = id, nobs = nobs_vector)
        return(result)
}