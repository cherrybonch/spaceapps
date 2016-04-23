source('C:/Projects/Space/spaceBulk.R')
source('C:/Projects/Space/space.R')

allPipeLine <- function(){
    asteroids <- readSpaceBulkRDS()
    asteroids_filter <- separateOnFilterBulk(asteroids)
    asteroids_filter_and_date <- separateOnDateBulk(asteroids_filter)
    return(extractPeriod(asteroids_filter_and_date))
}

extractPeriod <- function(k){
    result <- lapply(k, extractPeriodFilter)
}

extractPeriodFilter <- function(k_filter){
    k_filter <- lapply(k_filter, function(k_filter_date) {
        k_filter_date$Mag <- k_filter_date$Mag - min(k_filter_date$Mag)
        return(k_filter_date)
    })
    a <- sapply(k_filter, function(inner){return(acf(inner$Mag,lag.max = 120,plot = FALSE))})
    b <- sapply(k_filter, function(inner){return(sum(max(inner$Date)-min(inner$Date))/length(inner$Date))})
    for(i in 1:length(b)) {d <- which.min(a[,i][[1]][,,])*b[i]*2*24}
    return(mean(d))
}

# kaliope <- readRDS("rds/22_Kalliope.csv.rds")
# kaliope2 <- separateOnFilter(kaliope)
# kaliope3 <- separateOnDate(kaliope2)
# kaliope_C <- lapply(kaliope3, function(inner){ inner$Mag <- inner$Mag - min(inner$Mag)
# return(inner)})
# kaliope_r <- lapply(kaliope3[[2]], function(inner){ inner$Mag <- inner$Mag - min(inner$Mag)
# return(inner)})
# kaliope_v <- lapply(kaliope3[[3]], function(inner){ inner$Mag <- inner$Mag - min(inner$Mag)
# return(inner)})
# lapply(kaliope_C, function(inner){ggplot(inner, aes(x=Date,y=Mag))+geom_line()})
# lapply(kaliope_r, function(inner){ggplot(inner, aes(x=Date,y=Mag))+geom_line()})
# lapply(kaliope_v, function(inner){ggplot(inner, aes(x=Date,y=Mag))+geom_line()})
# sapply(kaliope_v, function(inner){acf(inner$Mag,lag.max = 120)})

# a <- sapply(kaliope_v, function(inner){acf(inner$Mag,lag.max = 120)})
# b <- sapply(kaliope_v, function(inner){return(sum(max(inner$Date)-min(inner$Date))/length(inner$Date))})
# d <- numeric()
# for(i in 1:length(b)) {d <- which.min(a[,i][[1]][,,])*b[i]*2*24}

