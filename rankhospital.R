#################################################
# Ranking hospitals by outcome in a state

#################################################
# "rankhospital" takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the "outcome-of-care-measures.csv" file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.
# For example call: rankhospital("MD", "heart failure", 5)




rankhospital <- function(state, outcome, num = "best") {
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
        
        hospital <- a[num, "Hospital.Name"]
        
        hospital
}