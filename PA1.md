---
title: "Project Assignment 1"
author: "Michael Lioznov"
date: "February 16, 2017"
output: html_document
---


---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```{r, echo=TRUE}
setwd("C:/Users/egdbb48/ReproducibleResearch/Project1")
activity <- read.csv(file="activity.csv",header = TRUE)
head(activity)

```
## Histogram of the total number of steps taken each day
```{r, echo=TRUE}
#Summarize steps per day
DaySteps <- aggregate(steps~date,activity,sum)
hist(as.numeric(DaySteps$steps),main="Distribution of Steps taken daily", xlab="Steps per day")

```


## What are mean and median total number of steps taken per day? The Mean number is   `r mean(DaySteps$steps)`. The Median number is    `r median(DaySteps$steps)` (see code below).

```{r, echo=TRUE}
#Mean of total steps taken daily
meanDaySteps <- mean(DaySteps$steps)
#Median of total steps taken daily
medianDaySteps <- median(DaySteps$steps)
```
The Mean number is   `r meanDaySteps`.
The Median number is    `r medianDaySteps`.

## What is the average daily activity pattern?

### 1. Time series plot of the 5-minute interval steps - averaged across all days 
```{r, echo=TRUE}
#Average steps for each interval averaged over all days
IntervalSteps <- aggregate(steps~interval,activity,mean)
# Plot for average steps per interval
plot(IntervalSteps$interval, IntervalSteps$steps, type="l",
     main="Average steps per interval over all observation days", 
     xlab="Interval", ylab="Average number of steps") 
```

### 2. Which 5-minute interval, on average across all the days, contains the maximum number of steps? Interval No `r IntervalSteps[as.numeric(which(IntervalSteps$steps == max(IntervalSteps$steps))),]$interval` is the interval with largest average number of steps (see code below).

```{r, echo=TRUE}
#Interval with the largest average number of steps:
MaxInterval <- IntervalSteps[as.numeric(which(IntervalSteps$steps == max(IntervalSteps$steps))),]$interval
```



## Imputing missing values

### 1. Total number of missing values in the dataset is `r sum(is.na(activity$steps))` (see code below)
```{r, echo=TRUE}
MissingNo <- sum(is.na(activity$steps))
```

### 2. Strategy for filling in all of the missing values in the dataset.

My strategy is to substitute undefined values with the average the corresponding interval.

### 3. Creating data set with imputed values.
```{r, echo=TRUE}
#Load raw data into new data frame where missing values will be substituted with averages per selected strategy 
imputed <- read.csv(file="activity.csv",header = TRUE) 
#Combine the new data frame with the data frame containing average daily steps for each interval into the interim data frame
Merged = merge(activity, IntervalSteps, by="interval", suffixes=c(".activity", ".IntervalSteps"))
#Use interim data frame to substitute missing values in the "raw"" data frame c
imputed$steps = ifelse(is.na(Merged$steps.activity), Merged$steps.IntervalSteps, Merged$steps.activity)
head(imputed)
```

### 4. Histogram of the total number of steps taken each day 
```{r, echo=TRUE}
DayStepsImputed <- aggregate(steps~date,imputed,sum)
#sum(DayStepsImputed$steps)
hist(as.numeric(DayStepsImputed$steps),main="Imputed data: Distribution for Steps taken daily", xlab="Steps per day")
```

###  Mean and median total number of steps taken per day are `r mean(DayStepsImputed$steps)` - mean and `r median(DayStepsImputed$steps)` - median (see code below)
```{r, echo=TRUE}
#median steps per day:
MedianSteps <- median(DayStepsImputed$steps)
#mean steps per day:
MeanSteps <- mean(DayStepsImputed$steps)
```

### Mean (average) number of steps did not change after imputing missing values, while the Median value increased - just as expected for the chosen imputing strategy.


## Are there differences in activity patterns between weekdays and weekends? (Yes there are!)
## Average number of steps taken per 5-minute interval across weekdays and weekends

### 1. Add new column indicating whether day of observation belongs to Week days or Week end to the data frame with filled in missing values.
```{r, echo=TRUE}
imputed$date <- as.Date(imputed$date)
#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
imputed$wDay <- factor((weekdays(imputed$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
```

### 2. Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 

```{r, echo=TRUE}
# Calculate average steps per interval across Weekdays and for Weekend days
IntervalStepsMeanWeekday <- aggregate(steps~interval, data=subset(imputed,wDay=='weekday'),mean)
IntervalStepsMeanWeekend <- aggregate(steps~interval, data=subset(imputed,wDay=='weekend'),mean)
# 2 figures arranged in 2 rows 
par(mfrow=c(2,1), mar = .1+ c(2,2,3,1))
plot(IntervalStepsMeanWeekday$interval, IntervalStepsMeanWeekday$steps, type="l", main="Average steps per interval over Weekdays",xlab="Interval", ylab="Average number of steps") 
plot(IntervalStepsMeanWeekend$interval, IntervalStepsMeanWeekend$steps, type="l", main="Average steps per interval over Weekend",xlab="Interval", ylab="Average number of steps") 
```


