rankhospital <- function(state, outcome, num = "best"){
        #Read outcome data
        x <- c("heart_attack", "heart_failure", "pneumonia")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")[, c(2,7,11,17,23)]
        colnames(df)[c(3,4,5)] <- x
        
        #chek that state and outcome are valid
        if (isFALSE(state %in% df$State)){
                stop("Invalid State")
        }
        else if (isFALSE(outcome %in% x)){
                stop("Invalid Outcome")
        }
        
        #return hospital name in that stage with the given rank
        
        z <- split(df, df$State)
        a <- data.frame(z[state])
        
        if (num > nrow(a)){
                        result <- NA
                }
        
        if (num == "worst"){
                if (outcome == "heart_attack"){
                        b <- a[order(a[,3], decreasing = TRUE, a[,1]), ]
                        result <- b[1,1]
                }
                else if (outcome == "heart_failure"){
                        b <- a[order(a[,4], decreasing = TRUE, a[,1]), ]
                        result <- b[1,1]
                }
                else if (outcome == "pneumonia"){
                        b <- a[order(a[,5], decreasing = TRUE, a[,1]), ]
                        result <- b[1,1]
                }
        }
        else {
                if (outcome == "heart_attack"){
                        b <- a[order(as.numeric(as.character(a[,3])),as.character(a[,1])),]
                        result <- b[num,1]
                }
                else if (outcome == "heart_failure"){
                        b <- a[order(as.numeric(as.character(a[,4])),as.character(a[,1])),]
                        result <- b[num,1]
                }
                else if (outcome == "pneumonia"){
                        b <- a[order(as.numeric(as.character(a[,5])),as.character(a[,1])),]
                        result <- b[num,1]
                }
        }
                
        as.character(result)
}