pollutantmean <-  function(directory, pollutant, id){
        files <- list.files(directory, full.names = TRUE)
        x <- files[id]
        dat <- data.frame()
        for (i in 1:length(x)){
               dat <- rbind(dat, read.csv(x[i]))
        }
        mean(dat[ ,pollutant], na.rm = TRUE)
}
