corr <- function(directory = "specdata", threshold = 0){
    cor_vector <- c()

    for(i in 1:332){
        x <- c()
        y <- c()
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
            else{
                nobs = nobs + 1
                x <- c(x, data[j,2])
                y <- c(y, data[j,3])
            }
        }

        if(nobs > threshold){
            cor_vector <- c(cor_vector, cor( x, y, use = "everything"))
        }
    }
    cor_vector
}