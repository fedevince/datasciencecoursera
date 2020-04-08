complete <- function(directory, id = 1:332){
        files <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for (i in 1:length(id)){
                x <- read.csv(files[id][i])
                y <- nrow(x[which(complete.cases(x)),])
                dat <- rbind(dat, c(id[i], y))
        }
        names(dat) <- c("id", "nobs")
        dat
}