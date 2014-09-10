##########################################
#  Finding the best hospital in a state


##########################################
# "best" takes two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the "outcome-of-care-measures.csv" file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the "Hospital.Name" variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. 




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