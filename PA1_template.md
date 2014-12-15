# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activityData<- read.csv(file='activity.csv', header = TRUE, sep=',')
aggregateDailySteps <- aggregate( steps ~ date , data = activityData , FUN = sum )
aggregateSumIntervalSteps <- aggregate( steps ~ interval , data = activityData , FUN = sum )
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
hist(aggregateDailySteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

- Mean

```r
meanDailySteps <- mean(aggregateDailySteps$steps)
meanDailySteps
```

```
## [1] 10766.19
```

- Median

```r
medianDailySteps <- median(aggregateDailySteps$steps)
medianDailySteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(aggregateSumIntervalSteps$interval, (aggregateSumIntervalSteps$steps/length(names(table(activityData$date)))), type='l',xlab = "Time Intervals", ylab= "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maximumStepsInterval = aggregateSumIntervalSteps$interval[which(aggregateSumIntervalSteps$steps == max(aggregateSumIntervalSteps$steps))]
maximumStepsInterval
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
numOfNA<-length(activityData$steps[is.na(activityData$steps)])
numOfNA
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filledMissingActivityData<-activityData
filledMissingActivityData$steps[is.na(filledMissingActivityData$steps)]<-mean(aggregateSumIntervalSteps$steps/length(names(table(activityData$date))))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- Histogram

```r
aggregateDailyStepsFilledData <- aggregate(steps ~ date , data = filledMissingActivityData , FUN = sum )
hist(aggregateDailyStepsFilledData$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

- Mean

```r
meanDailyStepsFilledData <- mean(aggregateDailyStepsFilledData$steps)
meanDailyStepsFilledData
```

```
## [1] 10581.01
```

- Median

```r
medianDailyStepsFilledData <- median(aggregateDailyStepsFilledData$steps)
medianDailyStepsFilledData
```

```
## [1] 10395
```
- The imputing missing data increases the estimates of the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice) 
filledMissingActivityData$weekday <- weekdays((as.POSIXct(filledMissingActivityData$date)))
filledMissingActivityData$weekday[filledMissingActivityData$weekday == 'Saturday' | filledMissingActivityData$weekday == 'Sunday']<- 'Weekend'
filledMissingActivityData$weekday[filledMissingActivityData$weekday != 'Saturday' & filledMissingActivityData$weekday != 'Sunday'
                                            & filledMissingActivityData$weekday != 'Weekend']<- 'Weekday'

aggregateSumIntervalStepsFilledData <- aggregate(steps ~ interval + weekday, data = filledMissingActivityData , FUN = sum )




aggregateSumIntervalStepsFilledData$weekday <-factor(aggregateSumIntervalStepsFilledData$weekday)


xyplot(aggregateSumIntervalStepsFilledData$steps~aggregateSumIntervalStepsFilledData$interval|aggregateSumIntervalStepsFilledData$weekday, 
       main="Activity patterns between weekdays and weekends", 
       ylab="Number of Steps", xlab="Time Interval",type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
