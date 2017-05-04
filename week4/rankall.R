rankall <- function(outcome, num = "best") {
    alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- subset.data.frame(x = alldata, select = c(7))
    states <- unique(states)
    ## Read outcome data
    if(outcome == "heart attack")
        alldata[, 11] <- as.numeric(alldata[, 11])
    else if(outcome == "heart failure")
        alldata[, 17] <- as.numeric(alldata[, 17])
    else if(outcome == "pneumonia")    
        alldata[, 23] <- as.numeric(alldata[, 23])    
    
    ## Check that state and outcome are valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    if(outcome == "heart attack")
        tempdata <- subset.data.frame(x = alldata, select = c(2, 7, 11))
    else if(outcome == "heart failure")
        tempdata <- subset.data.frame(x = alldata, select = c(2, 7, 17))
    else if(outcome == "pneumonia")    
        tempdata <- subset.data.frame(x = alldata, select = c(2, 7, 23))

    ## Return a data frame with the hospital names and the (abbreviated) state name
    tempdata <- tempdata[complete.cases(tempdata),]
    df1 <- split(tempdata, tempdata$State)

    if(num == "best"){
        ## nothing
        df1 <- lapply(df1, function(x){x[order(x[,3], x[,2], x[,1]), ]})
        df1 <- lapply(df1, function(x) cbind(x, seq.int(nrow(x))))
        df3 <- unsplit(df1, tempdata$State)
        df2 <- subset.data.frame(x = df3, subset = df3[,4] == 1)
    }else if(num == "worst"){
        df1 <- lapply(df1, function(x){x[order(-x[,3], x[,2], x[,1]), ]})
        df1 <- lapply(df1, function(x) cbind(x, seq.int(nrow(x))))
        df3 <- unsplit(df1, tempdata$State)
        df2 <- subset.data.frame(x = df3, subset = df3[,4] == 1)
    }else{
        df1 <- lapply(df1, function(x){x[order(x[,3], x[,2], x[,1]), ]})
        df1 <- lapply(df1, function(x) cbind(x, seq.int(nrow(x))))
        df3 <- unsplit(df1, tempdata$State)
        df2 <- subset.data.frame(x = df3, subset = df3[,4] == num)
    }
    df4 <- subset.data.frame(x = df2, select = c(1, 2))
    df4 <- df4[order(df4[,2], df4[,1]), ]
    df4 <- merge(x = states, y = df4, by.x = "State", by.y = "State", all.x = TRUE)
    df4    
}
