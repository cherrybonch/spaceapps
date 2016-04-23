setDirectory <- function(){
    setwd("C:\\Projects\\Space")
}

allPipeLine <- function(){
    asteroids <- readSpaceBulkRDS()
    asteroids_filter <- separateOnDateBulk(asteroids)
    asteroids_filter_and_date <- separateOnDateBulk(asteroids_filter)
    return(asteroids_filter_and_date) ##broken?
}

readSpaceBulkRDS <- function(){
    pathes <- list.files(path = "rds/", pattern = "rds$")
    k <- lapply(pathes, function(path){ 
        aster <- readRDS(paste0("rds/",path))
        return(aster)
    })
    return(k)
}

readSpaceBulk <- function(){
    pathes <- list.files(path = "csv/", pattern = "csv$")
    k <- lapply(pathes, function(path){ 
        aster <- read.csv(paste0("csv/",path))
        aster$Date <- aster$Date - min(aster$Date)
        return(aster)
    })
    return (k)
}

separateOnFilterBulk <- function(k){
    result <- lapply(k, function(k_inner){
        k_c <- k_inner[k_inner$Filter=="C",]
        k_r <- k_inner[k_inner$Filter=="R",]
        k_v <- k_inner[k_inner$Filter=="V",]
        return(list(k_c, k_r, k_v))
    })
    return(result)
}

separateOnDateSub <- function(k){
    res <- numeric()
    for(i in 2:dim(k)[1]){ res[i-1] <- k$Date[i]-k$Date[i-1]}
    stamp <- which(res>1)
    data_stamp <- list()
    if (length(stamp)==0){ data_stamp[[1]] <- k } else if (length(stamp)==1)
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

separateOnDateBulk <- function(k_outer){
    result <- lapply(k_outer, separateOnDate)
    return(result)
}

writeSpaceToRDS <- function(k){
    for(i in 1:length(k)){
        saveRDS(k[[i]],paste0("rds/", i,".rds"))
    }
    return(1)
}