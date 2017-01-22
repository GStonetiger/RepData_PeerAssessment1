# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
```


## Loading and preprocessing the data


```r
library(dplyr)
library(lubridate)
library(ggplot2)
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
act.by.date <- group_by(act, date)
total.steps.by.date <- summarize(act.by.date, steps = sum(steps, na.rm = TRUE))
total.steps.by.date
```

```
## # A tibble: 61 × 2
##          date steps
##        <date> <int>
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## # ... with 51 more rows
```

2. ... Make a histogram of the total number of steps taken each day


```r
ggplot(total.steps.by.date, aes(x=steps)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
m2 <- summarize(
    total.steps.by.date,
    mean.steps = mean(steps, na.rm = TRUE),
    median.steps = median(steps, na.rm = TRUE)
)
m2
```

```
## # A tibble: 1 × 2
##   mean.steps median.steps
##        <dbl>        <int>
## 1    9354.23        10395
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
act.by.interval <- group_by(act, interval)
mean.steps.by.interval <- summarize(act.by.interval
                                    , steps = as.integer(mean(steps, na.rm = TRUE)))
qplot(interval, steps, data = mean.steps.by.interval, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
subset(mean.steps.by.interval, steps == max(mean.steps.by.interval$steps))$interval
```

```
## [1] 835
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

**There are 2304 missing values in the dataset.**

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**I will use the mean for that 5-minute interval to impute missing values.**

3. Create a new dataset that is equal to the original dataset but with the missing
 data filled in.


```r
act.miss <- filter(act, is.na(steps))
act.miss <- inner_join(act.miss, mean.steps.by.interval, by = "interval")
act.miss <- select(act.miss, steps.y, date, interval) %>% rename(imputed.steps = steps.y)
act.imputed <- left_join(act, act.miss, by = c("date", "interval"))
act.imputed <- mutate(act.imputed, steps = ifelse(is.na(steps), imputed.steps, steps)) %>%
    select(interval, date, steps)
act.imputed
```

```
## # A tibble: 17,568 × 3
##    interval       date steps
##       <int>     <date> <int>
## 1         0 2012-10-01     1
## 2         5 2012-10-01     0
## 3        10 2012-10-01     0
## 4        15 2012-10-01     0
## 5        20 2012-10-01     0
## 6        25 2012-10-01     2
## 7        30 2012-10-01     0
## 8        35 2012-10-01     0
## 9        40 2012-10-01     0
## 10       45 2012-10-01     1
## # ... with 17,558 more rows
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
act.imputed.by.date <- group_by(act.imputed, date)
total.imputedsteps.by.date <- summarize(act.imputed.by.date
                                        , steps = sum(steps, na.rm = TRUE))

ggplot(total.imputedsteps.by.date, aes(x=steps)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
m2.imputed <-summarize(
    total.imputedsteps.by.date,
    mean.steps = mean(steps, na.rm = TRUE),
    median.steps = median(steps, na.rm = TRUE)
)
m2.imputed
```

```
## # A tibble: 1 × 2
##   mean.steps median.steps
##        <dbl>        <int>
## 1   10749.77        10641
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
m <- as.data.frame(rbind(m2, m2.imputed))
rownames(m) <- c("RawData", "ImputedData")
m
```

```
##             mean.steps median.steps
## RawData        9354.23        10395
## ImputedData   10749.77        10641
```

Yes, both mean and media change. The imputing missind data changes mean from **9354** to **10749**.


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(chron)
act.imputed <- mutate(act.imputed, isWeekend = as.factor(
    ifelse(is.weekend(date), "weekend", "weekday")))
act.imputed
```

```
## # A tibble: 17,568 × 4
##    interval       date steps isWeekend
##       <int>     <date> <int>    <fctr>
## 1         0 2012-10-01     1   weekday
## 2         5 2012-10-01     0   weekday
## 3        10 2012-10-01     0   weekday
## 4        15 2012-10-01     0   weekday
## 5        20 2012-10-01     0   weekday
## 6        25 2012-10-01     2   weekday
## 7        30 2012-10-01     0   weekday
## 8        35 2012-10-01     0   weekday
## 9        40 2012-10-01     0   weekday
## 10       45 2012-10-01     1   weekday
## # ... with 17,558 more rows
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
act.by.interval <- group_by(act.imputed, interval, isWeekend)
mean.imputedsteps.by.interval <- summarize(act.by.interval
                                           , steps = as.integer(mean(steps, na.rm = TRUE)))
qplot(interval, steps, data = mean.imputedsteps.by.interval
      , facets = isWeekend ~ ., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
