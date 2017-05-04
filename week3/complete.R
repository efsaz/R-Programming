complete <- function(directory = "specdata", id = 1:332){
    id_vector <- c()
    nob_vector <- c()
    for(i in id){
        id_=""
        nobs = 0
        if(i<10)
            id_ = paste("00", as.character(i), sep = "")
        else if(i<100)
            id_ = paste("0", as.character(i), sep = "")
        else
            id_ = as.character(i) 
        data <- read.csv(paste(directory, "/", id_, ".csv", sep = ""), header = TRUE, sep = ",", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "")
        
        for(j in seq_len(length(data[,4]))){
            if(is.na(data[j,2]) || is.na(data[j,3]))
                a <- 1
            else
                nobs = nobs + 1
        }
        id_vector <- c(id_vector, i)
        nob_vector <- c(nob_vector, nobs)
    }
    nobs_frame <- data.frame(id = id_vector, nobs = nob_vector)
    nobs_frame
}