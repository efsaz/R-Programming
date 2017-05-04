rankhospital <- function(state, outcome, num = "best") {
    alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Read outcome data
    if(outcome == "heart attack")
        alldata[, 11] <- as.numeric(alldata[, 11])
    else if(outcome == "heart failure")
        alldata[, 17] <- as.numeric(alldata[, 17])
    else if(outcome == "pneumonia")    
        alldata[, 23] <- as.numeric(alldata[, 23])    
    
    ## Check that state and outcome are valid
    if(nchar(state) != 2)
        stop("invalid state")
    else if(!state %in% alldata[, 7])
        stop("invalid state")
    
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")

    ## Return hospital name in that state with the given rank 30-day death rate
    if(outcome == "heart attack")
        tempdata = subset.data.frame(x = alldata, subset = State == state, select = c(2, 7, 11))
    else if(outcome == "heart failure")
        tempdata = subset.data.frame(x = alldata, subset = State == state, select = c(2, 7, 17))
    else if(outcome == "pneumonia")    
        tempdata = subset.data.frame(x = alldata, subset = State == state, select = c(2, 7, 23))

    tempdata <- tempdata[complete.cases(tempdata),]
    df1 <- tempdata[order(tempdata[,3], tempdata[,1]), ]
    df1$rank <- seq.int(nrow(df1))
    if(num == "best"){
        ## nothing
        df2 = subset.data.frame(x = df1, subset = df1[,4] == 1)
    }else if(num == "worst"){
        minrank <- max(df1[,4], na.rm=T)
        df2 = subset.data.frame(x = df1, subset = df1[,4] == minrank)
    }else
        df2 = subset.data.frame(x = df1, subset = df1[,4] == num)
    
    ## df1 <- tempdata[tempdata[,3] == minrate, ]  NA'lar sorun
    ## df2 <- tempdata[order(tempdata[,1]), ]
    df2[1,1]
    
    
}