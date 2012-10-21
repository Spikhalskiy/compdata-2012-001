rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        inner_func_state <- function(stateName, data, type, num) {
                filteredByStateData <- data[data$State == stateName, ]
                
                filteredByStateAndNullData <- filteredByStateData[!is.na(filteredByStateData[,type]), ]
                
                sortedAndFilteredData <- filteredByStateAndNullData[order(filteredByStateAndNullData[, type], filteredByStateAndNullData[, "Hospital.Name"]), ]
                
                if (num == "best") return(sortedAndFilteredData[0, "Hospital.Name"])
                else if (num == "worst") return(sortedAndFilteredData[nrow(sortedAndFilteredData), "Hospital.Name"])
                else if (num >= 1 & num <= nrow(sortedAndFilteredData)) return(sortedAndFilteredData[num, "Hospital.Name"])
                return(NA)
        }
        
        inner_func <- function(type, num) {
                data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
                
                data[, type] <- as.numeric(data[, type])
                
                valid.state.names <- names(table(data$State))
                
                hospital.names <- mapply(inner_func_state, valid.state.names, MoreArgs=list(data, type, num))
                
                result <- data.frame(hospital=hospital.names, state=valid.state.names)
                result <- result[order(result[, "hospital"]), ]
                return(result)
        }
        
        if (outcome == "heart attack") {
                return(inner_func("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", num))
        }
        
        if (outcome == "heart failure") {
                return(inner_func("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", num))
        }
        
        if (outcome == "pneumonia") {
                return(inner_func("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", num))
        }
        
        stop("invalid outcome");
}