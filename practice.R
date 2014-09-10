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

apply(data, 2, test_fun)

test_fun <- function(state, outcome, num) {
        a <- state[order(state[ ,outcome]), ]
        hospital <- a[num, c("Hospital.Name", "State")]
        hospital
}










# Programming Assignment Part 2 - "rankhospital.R"
## missing: handling ties, "best", "worst"

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




# Programming Assignment Part 1 - "best.R"
## missing: handling ties

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

b <- min(a[,11])
## Returns the minima of the 11th column in "a"

a[1, "Hospital.Name"]
## Return hospital name in that state with lowest 30-day death
## rate

11 = heart attack

17 = heart failure

23 = pneumonia

colname <- c(11,17,23)
names(colname) <- c("heart attack", "heart failure", "pneumonia")
## This workaround is with named numeric vector and subsetting it returns a named numeric vector

colname <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
## This workaround is with list, but subsetting it returns a list which is not compatible for my purpose

a <- matrix(nrow = 1, ncol = 3)
a <- cbind(11,17,23)
b <- c("heart attack", "heart failure", "pneumonia")
colnames(a) <- b
## This workaround is with matrix, subsetting it also returns a named numeric vector

 

state[order(state[ ,outcome]), ]
## order a data frame by the values in the 11th column
        
state[c(state[,11] == 12),]
## select the row where the value of the 11th column = 12


#funtion to sort a data frame by a column
mat.sort <- function(mat,n) 
{
        mat[rank(mat[,n]),] <- mat[c(1:nrow(mat)),]
        return(mat)
}

a <- matrix(rnorm(100),ncol=10)
mat.sort(a,1)
#