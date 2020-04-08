corr <- function(directory, threshold = 0){
        obs <- complete(directory)$nobs
        files <- list.files(directory, full.names = TRUE)
        x <- vector('numeric')
        for (i in 1:length(obs)){
                if (obs[i] > threshold){
                        z <- read.csv(files[i])
                        y <- z[which(complete.cases(z)), ]
                        x <- append(x,cor(y$nitrate, y$sulfate))
                }
                
          
        }
        x
}