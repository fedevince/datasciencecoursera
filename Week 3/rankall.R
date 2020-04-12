rankall <- function(outcome, num){
        #Read outcome data
        x <- c("heart_attack", "heart_failure", "pneumonia")
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")[, c(2,7,11,17,23)]
        colnames(df)[c(3,4,5)] <- x
        
        #Check that the state and outcome are valid
        
        if (isFALSE(outcome %in% x)){
                stop("Invalid Outcome")
        }
        
        #For each state find hospital with given rank
        z <- split(df, df$State)
        a <- lapply(z, function(x) x[order(x[ ,outcome], x[,1]), ])
        results <- character()
        results <- sapply(a, function(x) {
                results <- append(results, as.character(data.frame(x[num, ])[,1]))
                 
                
        })
        
        #Return data frame with hospital name and state name
        
        # results <- data.frame()
        # for (i in 1:length(b)){
        #         d <- data.frame(b[i])
        #         h <-  as.character(d[,1])
        #         results <- rbind(results, h)
        # }
        # names(results) <- c("Hospital")
        # results
        results
}