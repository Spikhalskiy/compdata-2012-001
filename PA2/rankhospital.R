rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        inner_func <- function(stateName, type, num) {
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
                
                data[, type] <- as.numeric(data[, type])
                
                filteredByStateData <- data[data$State == stateName, ]
                
                if (nrow(filteredByStateData) == 0) stop("invalid state")
                
                filteredByStateAndNullData <- filteredByStateData[!is.na(filteredByStateData[,type]), ]
                
                sortedAndFilteredData <- filteredByStateAndNullData[order(filteredByStateAndNullData[, type], filteredByStateAndNullData[, "Hospital.Name"]), ]
                
                if (num == "best") return(sortedAndFilteredData[0, "Hospital.Name"])
                else if (num == "worst") return(sortedAndFilteredData[nrow(sortedAndFilteredData), "Hospital.Name"])
                else if (num >= 1 & num <= nrow(sortedAndFilteredData)) return(sortedAndFilteredData[num, "Hospital.Name"])
                return(NA)
        }
        
        if (outcome == "heart attack") {
                return(inner_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", num))
        }
        
        if (outcome == "heart failure") {
                return(inner_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", num))
        }
        
        if (outcome == "pneumonia") {
                return(inner_func(state, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", num))
        }
        
        stop("invalid outcome");
}