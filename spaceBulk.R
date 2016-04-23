setDirectory <- function(){
    setwd("C:\\Projects\\Space")
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
        k_r <- k_inner[k_inner$Filter=="R",]
        k_c <- k_inner[k_inner$Filter=="C",]
        k_s <- k_inner[k_inner$Filter=="S",]
        k_v <- k_inner[k_inner$Filter=="V",]
        k_b <- k_inner[k_inner$Filter=="B",]
        k_i <- k_inner[k_inner$Filter=="I",]
        toexec <- paste0("list(",ifelse(nrow(k_r)!=0,"r=k_r",""),ifelse(nrow(k_c)!=0,",c=k_c",""),ifelse(nrow(k_s)!=0,",s=k_s",""),
                         ifelse(nrow(k_v)!=0,",v=k_v",""),ifelse(nrow(k_b)!=0,",b=k_b",""),ifelse(nrow(k_i)!=0,",i=k_i",""),")")
        toexec <- sub("\\(,","\\(",toexec)
        temp <- eval(parse(text=toexec))
        return(temp)
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
        return (lapply(k_outer, separateOnDateSub))
    } 
    else {
        return (separateOnDateSub(k))
    }
}

separateOnDateBulk <- function(k_outer){
    result <- lapply(k_outer, separateOnDate)
    return(result)
}

writeSpaceToRDS <- function(k,name){
    for(i in 1:length(k)){
        saveRDS(k[[i]],paste0("rds/", name[i],".rds"))
    }
    return(1)
}