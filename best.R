best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if(!state %in% data[ ,7]) {
                stop ("invalid state")
        }
        valid <- c("heart attack", "pneumonia", "heart failure")
        if(!outcome %in%  valid) {
                stop ("invalid outcome")
        }
        ## Check that state and outcome are valid
        
        state <- data[data$State == state, ]
        ## Select hospitals from the given state 
        
        colname <- c(11,17,23)
        names(colname) <- c("heart attack", "heart failure", "pneumonia")
        ## I need to tell R that if I give "heart attack" as outcome than refer to state[ ,11] etc.
        
        a <- state[order(state[ ,c(colname[outcome])]), ]
        ## Sort the given column in the data frame
        
        hospital <- a[1, "Hospital.Name"]
        
        hospital
}