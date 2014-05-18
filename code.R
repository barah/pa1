#load libraries
library(plyr)
library(lattice)

#read data
rawdata <- read.csv("activity/activity.csv")


#caculate daily mean 
data<-ddply(rawdata,~date,summarise,mean=mean(steps),sd=sd(steps),tot=sum(steps),median=median(steps))

#plot histogram of the  total number of steps taken each day
qplot(tot, data=data, geom="histogram")
dev.copy(png,"figures/plot1.png")
dev.off()

#print the mean and median total number of steps taken per day
data

#average daily activity pattern plot
xyplot(interval ~steps ,data=rawdata,type="l")
dev.copy(png,"figures/plot2.png")
dev.off()

#calucate the number of missing values
MissingValues = NROW(rawdata) - NROW(na.omit(rawdata))

#calcualte the mean number of steps per interval
IntervalMean <-  mean(rawdata$steps,na.rm=TRUE)

#fill the gap using interval mean 
rawdata$steps[is.na(rawdata$steps)]<- IntervalMean

#plot histogram of the  total number of steps taken each day after the gap filling
qplot(tot, data=data, geom="histogram")
dev.copy(png,"figures/plot3.png")
dev.off()

#prepare weekdays factor 
wd <- data$date
wd<- strptime(wd, "%Y-%m-%d")
wd <- weekdays(wd)
wd <- as.character(wd)

for (i in 1:length(wd)){
  if(wd[i]=="Saturday" | wd[i]=="Sunday") {
    wd[i] <- "weekend"
  } else {
    wd[i] <- "weekday"
  }
}
wd <- factor(wd)

#plot difference btweeen weekend and weekdays 
rawdata <- transform(rawdata,wd = factor(wd))
xyplot(steps ~interval |wd,data=rawdata,type="l", layout =c(1,2))
dev.copy(png,"figures/plot14.png")
dev.off()

