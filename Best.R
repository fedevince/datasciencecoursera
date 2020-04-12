best <- function(state, outcome){
        ##read outcome data
        x <- c("heart_attack", "heart_failure", "pneumonia")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")[, c(2,7,11,17,23)]
        colnames(df)[c(3,4,5)] <- x
        ##check state and outcome are valid
        if (isFALSE(state %in% df$State)){
                stop("Invalid State")
        }
        else if (isFALSE(outcome %in% x)){
                stop("Invalid Outcome")
        }
        ##Return hospital name with lowest 30-day death rate per outcome
        z <- split(df, df$State)
        a <- data.frame(z[state])
        
        if (outcome == "heart_attack"){
                b <- a[order(a[,3], as.character(a[,1])),]
                result <- b[1,1]
        }
        else if (outcome == "heart_failure"){
                b <- a[order(a[,4], as.character(a[,1])),]
                result <- b[1,1]
        }
        else if (outcome == "pneumonia"){
                b <- a[order(a[,5], as.character(a[,1])),]
                result <- b[1,1]
        }
        as.character(result)
}