---
title: "repData_assignment1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

```{r }
unzip("activity.zip")
data <- read.csv("activity.csv")
totalsteps <- aggregate(steps~date, data=data, sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```{r }
hist(totalsteps$steps, breaks=60, xlab="total steps per day", ylab="frequency")
```

2. Calculate and report the **mean** and **median** total number of steps taken per day

```{r }
MeanStepsPerDay<-mean(totalsteps$steps)
MedianStepsPerDay<-median(totalsteps$steps)
```
 *The **mean** total number of steps taken per day is
  `r mean(totalsteps$steps)` steps
 *The **median** total number of steps taken per day is
  `r median(totalsteps$steps)` steps
  
##What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
stepsinterval <- aggregate(steps~interval, data=data, FUN=mean, na.rm=TRUE)
with(stepsinterval, plot(steps~interval, type="l"))
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r }
stepsinterval$interval[which.max(stepsinterval$steps)]
```

##Imputing missing values

1.Calculate and report the total number of missing values in the dataset

```{r}
NumberOfMissingValue <- length(which(is.na(data$steps)))
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**My strategy for filling the missing data is calculating the mean steps per interval and fill it in the NAs**

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

first, i need to calculate the mean of each interval
```{r}
fillsteps<-function(interval){
   stepsinterval[stepsinterval$interval==interval,]$steps
}
```
second, just make a loop and fill NAs
```{r}
datafilled<-data   #create a new dataset
count=0    #the number we filled
for(i in 1:nrow(datafilled)){
   if(is.na(datafilled[i,]$steps)){
     datafilled[i,]$steps=fillsteps(datafilled[i,]$interval)
     count= count+1
   }
}
```

4.Make a histogram of the total number of steps taken each day.
```{r}
totalstepswithoutNA <- aggregate(steps~date, data=datafilled, sum)
hist(totalstepswithoutNA$steps,breaks=60, xlab="total steps per day", ylab="frequency")
```

Calculate and report the mean and median total number of steps taken per day. 

```{r }
MeanStepsPerDay<-mean(totalstepswithoutNA$steps)
MedianStepsPerDay<-median(totalstepswithoutNA$steps)
```
*The **mean** total number of steps taken per day is
  `r mean(totalstepswithoutNA$steps)` steps
 *The **median** total number of steps taken per day is
  `r median(totalstepswithoutNA$steps)` steps

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

the mean steps shows no difference because we fill the mean data into NAs,
however, median steps seems a little bit higher than raw data.

##Are there differences in activity patterns between weekdays and weekends?

```{r }
datafilled$DayType <-  ifelse(as.POSIXlt(datafilled$date)$wday %in% c(0,6), 'weekend', 'weekday')

datafilled1<-aggregate(steps~interval+DayType, data=datafilled,mean)
library(ggplot2)
ggplot(datafilled1, aes(interval,steps))+
  geom_line()+
  facet_grid(DayType~.)+
    xlab("5-minutes interval")+
  ylab("average steps")
```
