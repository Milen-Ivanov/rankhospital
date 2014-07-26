best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if(!state %in% data[ ,7]) {
                message ("invalid state")
        }
        valid <- c("heart attack", "pneumonia", "heart failure")
        if(!outcome %in%  valid) {
                message ("invalid outcome")
        }
        ## Check that state and outcome are valid
        
        state <- data[data$State == state, ]
        ## Select hospitals from the given state 
        
        colname <- c(11,17,23)
        names(colname) <- c("heart attack", "heart failure", "pneumonia")
        ## I need to tell R that if I give "heart attack" as outcome than refer to state[ ,11] etc.
        
        a <- state[order(state[ ,c(colname[outcome])]), ]
        ## Sort the given column in the data frame
        a

}

min(a[,11])
## Returns the minima of the 11th column in "a"

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