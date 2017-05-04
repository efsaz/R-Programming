pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332){
    generalsum=0
    totalrows=0
    
    for(i in id){
        id_=""
        if(i<10)
            id_ = paste("00", as.character(i), sep = "")
        else if(i<100)
            id_ = paste("0", as.character(i), sep = "")
        else
            id_ = as.character(i) 
        data <- read.csv(paste(directory, "/", id_, ".csv", sep = ""), header = TRUE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "")
        if(pollutant == "sulfate")
            temp <- data[,2]
        else if(pollutant == "nitrate") 
            temp <- data[,3]
        else
            return(0)
        
        generalsum = generalsum + sum(temp[!is.na(temp)]) 
        totalrows = totalrows + length(temp[!is.na(temp)])
    }
    
    mean_ <- (generalsum / totalrows)
    mean_

}

