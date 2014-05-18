========================================

```r
activity <- read.csv("activity.csv")
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
## using plyr to aggregate steps taken each day
sumOfSteps <- ddply(activity, .(date), summarize, stepSum = sum(steps))
hist(sumOfSteps$stepSum, xlab = "Total steps taken per day", ylab = "Frequency", 
    main = "Histogram of total number of steps taken each day", col = "red")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r

## computing mean and median
mean(sumOfSteps$stepSum, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(sumOfSteps$stepSum, na.rm = TRUE)
```

```
## [1] 10765
```

```r

averagedSteps <- ddply(activity, .(interval), summarize, meanStepsPerInterval = mean(steps, 
    na.rm = TRUE))
plot(averagedSteps$interval, averagedSteps$meanStepsPerInterval, type = "l", 
    xlab = "Interval", ylab = "Average Steps Per Interval", main = "Daily average of steps per interval")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r

## finding out which interval has max steps on average
maxStepsInterval <- averagedSteps[which.max(averagedSteps[, 2]), 1]
maxStepsInterval
```

```
## [1] 835
```

```r

## fixing missing values by replacing NAs with mean steps for that interval
numberOfMissingValues <- sum(is.na(activity$steps))
len <- length(activity$steps)
adjustedActivity <- activity

for (i in 1:len) {
    if (is.na(activity[i, 1])) {
        interval <- activity[i, 3]
        meanInterval <- averagedSteps[averagedSteps$interval == interval, 2]
        adjustedActivity[i, 1] <- meanInterval
        
    }
    
}
sumOfStepsAdjusted <- ddply(adjustedActivity, .(date), summarize, stepSum = sum(steps))
hist(sumOfStepsAdjusted$stepSum, xlab = "Total steps taken per day (adjusted)", 
    ylab = "Frequency", main = "Adjusted histogram of total number of steps taken each day", 
    col = "red")

## calculating adjusted mean and median
mean(sumOfStepsAdjusted$stepSum, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(sumOfStepsAdjusted$stepSum, na.rm = TRUE)
```

```
## [1] 10766
```

```r

## adding a factor to store if the day is a weekday or weekend
dayOfWeekVector <- factor(levels = c("weekday", "weekend"))
for (i in 1:len) {
    dayOfWeek <- weekdays(as.Date(adjustedActivity[i, 2]))
    if (dayOfWeek == "Monday" | dayOfWeek == "Tuesday" | dayOfWeek == "Wednesday" | 
        dayOfWeek == "Thursday" | dayOfWeek == "Friday") {
        dayOfWeekVector[length(dayOfWeekVector) + 1] <- as.factor(as.character("weekday"))
    } else {
        dayOfWeekVector[length(dayOfWeekVector) + 1] <- as.factor(as.character("weekend"))
    }
}
adjustedActivity$DayOfWeek <- as.factor(dayOfWeekVector)
adjustedAverageSteps <- ddply(adjustedActivity, .(interval, DayOfWeek), summarize, 
    meanStepsPerInterval = mean(steps, na.rm = TRUE))
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-13.png) 

```r
xyplot(meanStepsPerInterval ~ interval | DayOfWeek, data = adjustedAverageSteps, 
    layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-14.png) 

