################################################
# Ranking hospitals in all states

################################################
# "rankall" takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
# The function reads the "outcome-of-care-measures.csv" file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. 




rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        valid <- c("heart attack", "pneumonia", "heart failure")
        if(!outcome %in%  valid) {
                stop ("invalid outcome")
        }
        ## Check that outcome is valid
        
        colname <- c(11,17,23)
        names(colname) <- c("heart attack", "heart failure", "pneumonia")
        ## I need to tell R that if I give "heart attack" as outcome than refer to state[ ,11] etc.
        
        
        stateall <- data[,"State"]
        statelist <- unique(stateall)
        ## Extract the state names form the data
        
        m <- matrix(nrow=1, ncol=2)
        colnames(m) <- c("hospital","state")
        
        for (i in statelist) {
                state <- data[data$State == i, ]
                a <- state[order(state[ ,c(colname[outcome])]), ]
                hospital <- a[num, c("Hospital.Name", "State")]
                colnames(hospital) <- c("hospital","state")
                m <- rbind(m, hospital)
        }
        
        m
}