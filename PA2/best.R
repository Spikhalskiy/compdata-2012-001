best <- function(state, outcome) {
        ## Read outcome data

        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        best_func <- function(stateName, type) {
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
                
                data[, type] <- as.numeric(data[, type])
                
                filteredByStateData <- data[data$State == stateName, ]
                
                if (nrow(filteredByStateData) == 0) stop("invalid state")
                
                min_value <- min(filteredByStateData[,type], na.rm = TRUE)
                
                hospital_names <- filteredByStateData[filteredByStateData[,type] == min_value, "Hospital.Name"]
                
                return (sort(hospital_names))[0]
        }
        
        if (outcome == "heart attack") {
                return(best_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
        }
        
        if (outcome == "heart failure") {
                return(best_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
        }
        
        if (outcome == "pneumonia") {
                return(best_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        }
        
        stop("invalid outcome");
}
