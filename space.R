setDirectory <- function(){
    setwd("C:\\")
}

readSpace <- function(){
    k <- read.csv("22_Kalliope.csv",colClasses = c("numeric","numeric","factor","numeric","factor","numeric","numeric","numeric"))
    return (k)
}

getSummary <- function(k){
    table(k$Filter)
    table(k$Magband)
    summary(k[k$Filter=="C",])
    summary(k[k$Filter=="R",])
    summary(k[k$Filter=="V",])
}

separateOnFilter <- function(k){
    k_c <- k[k$Filter=="C",]
    k_r <- k[k$Filter=="R",]
    k_v <- k[k$Filter=="V",]
    return(list(k_c, k_r, k_v))
}

separateOnDateSub <- function(k){
    res <- numeric()
    for(i in 2:dim(k)[1]){ res[i-1] <- k$Date[i]-k$Date[i-1]}
    stamp <- which(res>1)
    data_stamp <- list()
    if (length(stamp)==0){ data_stamp[[1]] <- k_c } else if (length(stamp)==1)
    { 
        data_stamp[[1]] <- k[1:stamp,]
        data_stamp[[2]] <- k[(stamp+1):dim(k)[1],]
    } else if (length(stamp)==2){ 
        data_stamp[[1]] <- k[1:stamp[1],]
        data_stamp[[2]] <- k[(stamp[1]+1):stamp[2],]
        data_stamp[[3]] <- k[(stamp[2]+1):dim(k)[1],]
    } else {
        data_stamp[[1]] <- k[1:stamp[1],]
        for(counter in 1:(length(stamp)-2)){
            data_stamp[[counter+1]] <- k[(stamp[counter+1]+1):stamp[counter+2],]
        }
        data_stamp[[counter+2]] <- k[(stamp[counter+2]+1):dim(k)[1],]
    }
    return(data_stamp)
}

separateOnDate <- function(k_outer){
    if(class(k_outer)=="list") {
        return (sapply(k_outer, separateOnDateSub))
    } 
    else {
        return (separateOnDateSub(k))
    }
}