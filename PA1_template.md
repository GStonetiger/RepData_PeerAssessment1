# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
library(lubridate)
act <- read.csv2("activity.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
act <- tbl_df(act) 
act <- suppressWarnings(mutate(act, date = ymd(date)))
act
```

```
## # A tibble: 17,568 × 3
##    steps       date interval
##    <int>     <date>    <int>
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

## What is mean total number of steps taken per day?
NOTE: For this part of the assignment, you can ignore the missing values in the dataset.
 
1. Calculate the total number of steps taken per day


```r
actByDate <- group_by(act, date)
stepsByDate <- summarize(actByDate, totalSteps = sum(steps, na.rm = TRUE))
stepsByDate
```

```
## # A tibble: 61 × 2
##          date totalSteps
##        <date>      <int>
## 1  2012-10-01          0
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08          0
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

2. ... Make a histogram of the total number of steps taken each day


```r
hist(stepsByDate$totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
summarize(
    stepsByDate,
    meanSteps = mean(totalSteps, na.rm = TRUE),
    medianSteps = median(totalSteps, na.rm = TRUE)
)
```

```
## # A tibble: 1 × 2
##   meanSteps medianSteps
##       <dbl>       <int>
## 1   9354.23       10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# TODO
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# TODO
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# TODO
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# TODO
```

3. Create a new dataset that is equal to the original dataset but with the missing
 data filled in.


```r
# TODO
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# TODO
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# TODO
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
# TODO
```
