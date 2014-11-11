---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```
## [1] "E:/Course Era/John Hopkins - Data Science Specialization/5 - Reproducible Research/project-1"
```



```r
getwd()
```

```
## [1] "E:/Course Era/John Hopkins - Data Science Specialization/5 - Reproducible Research/project-1"
```

```r
library(ggplot2)
library(lattice)
library(knitr)
library(markdown)
```

## Loading and preprocessing the data


```r
# read file
data <- read.table(unz("repdata-data-activity.zip", "activity.csv"), head=TRUE,sep=",", na.strings = "NA")
```

```
## Warning: cannot open zip file 'repdata-data-activity.zip'
```

```
## Error: cannot open the connection
```

```r
# process data
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
```

```
## Error: object of type 'closure' is not subsettable
```


## What is mean total number of steps taken per day?


```r
# calculate total number of steps each day
stepsPerDay <- setNames(aggregate(data$steps, list(data$date), sum), c("interval", "steps"))
```

```
## Error: object of type 'closure' is not subsettable
```

```r
# histogram of the total number of steps taken each day
qplot(steps, data=stepsPerDay, geom="histogram", binwidth=1000)
```

```
## Error: object 'stepsPerDay' not found
```

```r
# mean of total steps
mean(stepsPerDay$steps, na.rm=TRUE)
```

```
## Error: object 'stepsPerDay' not found
```

```r
# median of total steps
median(stepsPerDay$steps, na.rm=TRUE)
```

```
## Error: object 'stepsPerDay' not found
```



## What is the average daily activity pattern?

```r
# calculate average number of steps taken, averaged across all days (y-axis)
avgsteps <- setNames(aggregate(data$steps, list(data$interval), mean, na.rm=TRUE), c("interval", "steps"))
```

```
## Error: object of type 'closure' is not subsettable
```

```r
# time peries plot
ggplot(avgsteps, aes(x=interval, y=steps, col=steps)) + geom_line() +xlab("Interval") +ylab("Average Steps")
```

```
## Error: object 'avgsteps' not found
```

```r
# interval with maximum number of steps
avgsteps[avgsteps$steps==max(avgsteps$steps),]
```

```
## Error: object 'avgsteps' not found
```


## Imputing missing values


```r
# count missing values
length(which(is.na(data)))
```

```
## Warning: is.na() applied to non-(list or vector) of type 'closure'
```

```
## [1] 0
```

```r
# Replace NA with average of that interval
for(i in 1:nrow(data)){
  intv <- data$interval[i]
  stp <- data$steps[i]
  if(is.na(stp)){
    newstp <- subset(avgsteps, interval==intv)[,2]
    data$steps[i] = newstp
  }
}
```

```
## Error: argument of length 0
```

```r
stepsSumPerDay <- setNames(aggregate(data$steps, list(data$date), sum), c("interval", "steps"))
```

```
## Error: object of type 'closure' is not subsettable
```

```r
# histogram of the total number of steps taken each day 
ggplot(stepsSumPerDay, aes(x=steps)) + geom_histogram(aes(fill = ..count..), breaks=seq(0,20000, by=800), position="dodge")
```

```
## Error: object 'stepsSumPerDay' not found
```

```r
#  mean total number of steps taken per day
mean(stepsSumPerDay$steps)
```

```
## Error: object 'stepsSumPerDay' not found
```

```r
#  median total number of steps taken per day
median(stepsSumPerDay$steps)
```

```
## Error: object 'stepsSumPerDay' not found
```

Mean and median values are roughly equal even after replacing NA's with average of the intervals.

## Are there differences in activity patterns between weekdays and weekends?


```r
# add weekday/weekend column
data$day <- as.factor(ifelse(weekdays(data$date) %in% c("Saturday","Sunday"), "weekend", "weekday")) 
```

```
## Error: object of type 'closure' is not subsettable
```

```r
# Mean of "weekday"
w1 <- subset(data, day=="weekday")
```

```
## Error: object 'day' not found
```

```r
w1 <- aggregate(w1$steps, list(w1$interval), mean) 
```

```
## Error: object 'w1' not found
```

```r
w1$day <- "weekday"
```

```
## Error: object 'w1' not found
```

```r
w2 <- subset(data, day=="weekend")
```

```
## Error: object 'day' not found
```

```r
w2 <- aggregate(w2$steps, list(w2$interval), mean) 
```

```
## Error: object 'w2' not found
```

```r
w2$day <- "weekend"
```

```
## Error: object 'w2' not found
```

```r
# Mean of "weekends"
w3 <- rbind(w1, w2)
```

```
## Error: object 'w1' not found
```

```r
colnames(w3) <- c("interval", "steps", "day")
```

```
## Error: object 'w3' not found
```

```r
# panel plot
xyplot(steps~interval | day, data=w3, type="l", layout=c(1,2))
```

```
## Error: object 'w3' not found
```



