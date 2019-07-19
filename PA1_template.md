---
title: "Untitled"
author: "Shy"
date: "7/19/2019"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---

# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
inputData <- activity
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```


## What is mean total number of steps taken per day?

**1.Histogram of the total number of steps taken each day**

```r
stepsEachDay <- aggregate(inputData$steps, by = list(inputData$date), FUN = sum)
```

```
## Error in aggregate(inputData$steps, by = list(inputData$date), FUN = sum): object 'inputData' not found
```

```r
hist(stepsEachDay$x, breaks=10, main = "Histogram of total steps per day",xlab="steps/day")
```

```
## Error in hist(stepsEachDay$x, breaks = 10, main = "Histogram of total steps per day", : object 'stepsEachDay' not found
```

**2.The mean and median of total number of steps taken per day**

```r
mean(stepsEachDay$x,na.rm=TRUE)
```

```
## Error in mean(stepsEachDay$x, na.rm = TRUE): object 'stepsEachDay' not found
```

```r
median(stepsEachDay$x,na.rm=TRUE)
```

```
## Error in median(stepsEachDay$x, na.rm = TRUE): object 'stepsEachDay' not found
```

```r
summary(stepsEachDay)
```

```
## Error in summary(stepsEachDay): object 'stepsEachDay' not found
```

From the above summary, 
- **Mean = 10766** 
- **Median = 10765** 

## What is the average daily activity pattern?

**1.Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
avgDailySteps<-tapply(inputData$steps,inputData$interval,mean, na.rm = TRUE)
```

```
## Error in tapply(inputData$steps, inputData$interval, mean, na.rm = TRUE): object 'inputData' not found
```

```r
plot(unique(inputData$interval),avgDailySteps,type="l",xlab="Interval",ylab="average steps")
```

```
## Error in unique(inputData$interval): object 'inputData' not found
```

**2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
which.max(avgDailySteps)
```

```
## Error in which.max(avgDailySteps): object 'avgDailySteps' not found
```

```r
max(avgDailySteps)
```

```
## Error in eval(expr, envir, enclos): object 'avgDailySteps' not found
```
The maximum number of average steps of **206.2** is at interval **835** with index **104**.

## Imputing missing values
**1.Calculating the total number of missing values**


```r
summary(inputData)
```

```
## Error in summary(inputData): object 'inputData' not found
```
There are total of **2304** missing values in the steps column of the dataset.

**2.Strategy for filling the missing values**

Considered using the sapply() with a custome function() which will calculate the median of missing values.


```r
newInputData <- data.frame(inputData)
```

```
## Error in data.frame(inputData): object 'inputData' not found
```

```r
library(plyr)
impute.med <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
inputDataWithOutNA <- sapply(inputData, function(x){
     if(is.numeric(x)){
         impute.med(x)
     } else {
         x
     }
 }
 )
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'inputData' not found
```
Filled the NA values with median values.

**3.New dataset that is equal to the original dataset with the missing data filled in.** 

**Original Dataset**

```r
summary(inputData)
```

```
## Error in summary(inputData): object 'inputData' not found
```

```r
head(inputData)
```

```
## Error in head(inputData): object 'inputData' not found
```
**Filled in Dataset**

```r
newInputData <- data.frame(inputDataWithOutNA[,1],inputData$date,inputData$interval)
```

```
## Error in data.frame(inputDataWithOutNA[, 1], inputData$date, inputData$interval): object 'inputDataWithOutNA' not found
```

```r
colnames(newInputData) <- c("steps", "date","interval")
```

```
## Error in colnames(newInputData) <- c("steps", "date", "interval"): object 'newInputData' not found
```

```r
summary(newInputData)
```

```
## Error in summary(newInputData): object 'newInputData' not found
```

```r
head(newInputData)
```

```
## Error in head(newInputData): object 'newInputData' not found
```

**4.Histogram of the total number of steps taken each day**

```r
newStepsEachDay <- aggregate(newInputData$steps, by = list(newInputData$date), FUN = sum)
```

```
## Error in aggregate(newInputData$steps, by = list(newInputData$date), FUN = sum): object 'newInputData' not found
```

```r
summary(newStepsEachDay$x)
```

```
## Error in summary(newStepsEachDay$x): object 'newStepsEachDay' not found
```

```r
hist(newStepsEachDay$x, breaks=10, main = "Histogram of total steps per day",xlab="steps/day")
```

```
## Error in hist(newStepsEachDay$x, breaks = 10, main = "Histogram of total steps per day", : object 'newStepsEachDay' not found
```

**The mean and median of total number of steps taken per day**

```r
mean(newStepsEachDay$x,na.rm=TRUE)
```

```
## Error in mean(newStepsEachDay$x, na.rm = TRUE): object 'newStepsEachDay' not found
```

```r
median(newStepsEachDay$x,na.rm=TRUE)
```

```
## Error in median(newStepsEachDay$x, na.rm = TRUE): object 'newStepsEachDay' not found
```

```r
summary(newStepsEachDay)
```

```
## Error in summary(newStepsEachDay): object 'newStepsEachDay' not found
```

From the above summary, 
- **Mean = 9354** 
- **Median = 10395** 

We can observe a chnage in Mean & Median values when we have considered the missing values.

## Are there differences in activity patterns between weekdays and weekends?
**1.Creating a new factor variable in the dataset with two levels indicating whether a given date is a weekday or weekend day.** 


```r
newInputData$date<-as.POSIXlt(newInputData$date)
```

```
## Error in as.POSIXlt(newInputData$date): object 'newInputData' not found
```

```r
newInputData$dayLogical<-factor(weekdays(newInputData$date) == "Sunday" | weekdays(newInputData$date) == "Saturday", levels=c(TRUE,FALSE),labels=c("Weekend","Weekday") )
```

```
## Error in weekdays(newInputData$date): object 'newInputData' not found
```

```r
str(newInputData)
```

```
## Error in str(newInputData): object 'newInputData' not found
```
**2.Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**

To work towards the solution for this problem, First an average of steps on Date is taken and it is factorized to two levels and are labeled as Weekday and Weekend day. Finally a Lattice plot is drawn.


```r
library(lattice)
newAvgStepsPerDay <- aggregate(newInputData$steps, by = list(inputData$date), FUN = mean)
```

```
## Error in aggregate(newInputData$steps, by = list(inputData$date), FUN = mean): object 'newInputData' not found
```

```r
newAvgStepsPerDay$Group.1 <- as.POSIXlt(newAvgStepsPerDay$Group.1)
```

```
## Error in as.POSIXlt(newAvgStepsPerDay$Group.1): object 'newAvgStepsPerDay' not found
```

```r
newAvgStepsPerDay$dayLogical <- factor(weekdays(newAvgStepsPerDay$Group.1) == "Sunday" | weekdays(newAvgStepsPerDay$Group.1) == "Saturday", levels=c(TRUE,FALSE),labels=c("Weekend","Weekday") )
```

```
## Error in weekdays(newAvgStepsPerDay$Group.1): object 'newAvgStepsPerDay' not found
```

```r
xyplot(newAvgStepsPerDay$x ~ unique(newInputData$interval) | newAvgStepsPerDay$dayLogical, type="l",layout=(c(1,2)),xlab="Interval",ylab="Number of steps")
```

```
## Error in eval(modelRHS.vars[[i]], data, env): object 'newAvgStepsPerDay' not found
```
 
