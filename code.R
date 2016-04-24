fls <- list.files("asteroids",pattern = "^([0-9]{1,2})_([[:alpha:]]+).csv")[1:10]
library(snow)
library(dplyr)
library(plotly)
cl <- makeCluster(4)
#clusterExport(cl, "myvar") 

DateOrigin <- 2451545

lightcurve <- parLapply(cl, fls, function(filename){
    data <- read.csv(paste0("asteroids/",filename))
    data$Date <- data$Date - min(data$Date)
    data <- data[order(data$Date),]
    #data$Mag[data$Magband=="V"] <- data$Mag[data$Magband=="V"]+0.36
    for (i in unique(data$Obs_num)){
        four <- fft(data$Mag[data$Obs_num==i])
        mod_fourier <- (Re(four)^2+Im(four)^2)[1:5]
        if (mod_fourier[3]/mod_fourier[2] > 2) 
            data <- data[data$Obs_num!=i,]
    }
    return(data)
})

data=data.frame(lightcurve[[9]])
pl <- plot_ly(data=data[data$Obs_num==1,],x=Date,y=Mag)
pl

first_day <- data[data$Obs_num==2,]
four <- fft(first_day$Mag)
t <- mean(abs(first_day$Date[1:(nrow(first_day)-1)]-first_day$Date[2:(nrow(first_day))]))
pl <- plot_ly(x=seq(nrow(first_day))/t,y=log(1+(log(1+Re(four)^2+Im(four)^2))))
pl
plot(density(x=log(1+(log(1+Re(four)^2+Im(four)^2)))))
den <- density(x=log(1+(log(1+Re(four)^2+Im(four)^2))))

max(den$y)       
den$x[den$y==max(den$y)]

asteroid_period <- read.csv("~/SpaceApps/docs/spaceapps-master/asteroid_period.csv")
asteroid_period <- asteroid_period[!is.na(asteroid_period$V1),]
asteroid_period$V2 <- gsub("-","_",asteroid_period$V2)
asteroid_period$y<-NA
inter <- intersect(unlist(strsplit(list.files("asteroids"),".csv")),asteroid_period$V2)
asteroid_new <- asteroid_period[is.element(asteroid_period$V2,inter),]

asteroid_new$y <- sapply(inter,function(filename){
    print(filename)
    data <- read.csv(paste0("asteroids/",filename,".csv"))
    if(nrow(data)>0){
        data$Date <- data$Date - min(data$Date)
        data <- data[order(data$Date),]
        first_day <- data[data$Obs_num==1,]
        if(nrow(first_day)>1){
            four <- fft(first_day$Mag)
            t <- mean(abs(first_day$Date[1:(nrow(first_day)-1)]-first_day$Date[2:(nrow(first_day))]))
            den <- density(log(1+(log(1+Re(four)^2+Im(four)^2))))
            return(den$x[den$y==max(den$y)])
        }
        else return(NA)
    }
    else return(NA)
})

asteroid_new$x <- sapply(inter,function(filename){
    print(filename)
    data <- read.csv(paste0("asteroids/",filename,".csv"))
    if(nrow(data)>0){
        data$Date <- data$Date - min(data$Date)
        data <- data[order(data$Date),]
        first_day <- data[data$Obs_num==1,]
        if(nrow(first_day)>1){
            four <- fft(first_day$Mag)
            t <- mean(abs(first_day$Date[1:(nrow(first_day)-1)]-first_day$Date[2:(nrow(first_day))]))
            den <- density(log(1+(log(1+Re(four)^2+Im(four)^2))))
            return(max(den$y))
        }
        else return(NA)
    }
    else return(NA)
})
colnames(asteroid_new) <- c("Period","Name","x","y")
rownames(asteroid_new) <- asteroid_new$Name
asteroid_new <- asteroid_new[-2]
asteroid_new <- asteroid_new[!is.na(asteroid_new$y),]
new <- asteroid_new[-c("2802_Weisel", "3773_Smithonian", "310_Margarita"),]
asteroid_new$clust <- Do.Clustering(asteroid_new)
