---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```


## Loading and preprocessing the data

Unziping the file and loading the data. Transforming the type of variable "date"
into class date.


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

Calculating the total number of steps taken per day.  
The histogram of the total number of steps taken per day is showed below.  


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
totalSteps <- data %>%
        group_by(date) %>%
        summarise(steps = sum(steps, na.rm = TRUE))
hist(totalSteps$steps, xlab = "Steps", 
     main = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/meanSteps-1.png)<!-- -->

```r
meanOfSteps <- mean(totalSteps$steps)
medianOfSteps <- median(totalSteps$steps)
```

The mean and median total number of steps taken per day are 
9,354.23 and 
10,395 respectively.

## What is the average daily activity pattern?

The average daily active pattern is showed below. The x-axis is the 5-minute
interval in one day, the y-axis is the average number of steps taken at that 
5-minute interval across all days.


```r
byInterval <- data %>%
        group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
with(byInterval, plot(interval, steps, type = "l",
                      xlab = "5-min interval",
                      ylab = "Average number of steps",
                      main = "The average daily activity pattern"))
```

![](PA1_template_files/figure-html/avgPattern-1.png)<!-- -->

```r
max <- byInterval$interval[which.max(byInterval$steps)]
```

The 835th 5-minute interval contains the maximum number of steps in daily 
actvity.


## Imputing missing values

Here, I filled the missing values using the mean for that 5-minute interval.


```r
NAs <- sum(!complete.cases(data))

dataFilled <- data
```


----------

<div class="alert alert-danger">Trevor's comment</div>

This is what you wanted to do but failed.


```r
repl = function(row) {
    if(is.na(row[1])) {
        interval.i = which(byInterval$interval == row[3])
        row[1] = byInterval$steps[interval.i]
    }
}

apply(dataFilled[1:3,], 1, repl)
```

Let's first take a look at what is the third element of each row. And from below you see that they are characters. The fact is, `apply` parse each row into the callback function, as a vector. And a vector can only bave one type, so it is coerced into character.


```r
apply(data[1:3,], 1, function(row){
    row[3]
})
```

```
##    1    2    3 
## " 0" " 5" "10"
```

The tricky thing of R is, `5 == "5"` actually gives you `TRUE`, so that's why it works for you sometimes. But if you do `5 == " 5"'`, it is `FALSE`. So,


```r
which(byInterval$interval == " 5")
```

```
## integer(0)
```

gives you this thing above, and that's why you are getting that error message.

But, in order to get what you want, there is a way easier way. See below:


```r
dataFilled = data %>%
    group_by(interval) %>%
    mutate(steps = ifelse(
      is.na(steps),
      mean(steps, na.rm = TRUE),
      steps
    ))
dataFilled
```

```
## # A tibble: 17,568 x 3
## # Groups:   interval [288]
##     steps date       interval
##     <dbl> <date>        <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # … with 17,558 more rows
```

Done

----------


```r
for(i in seq(nrow(dataFilled))){
        if(is.na(dataFilled$steps[i])){
                interval.i <- which(byInterval$interval == dataFilled$interval[i])
                dataFilled$steps[i] <- byInterval$steps[interval.i]
        }
}

totalSteps <- dataFilled %>%
        group_by(date) %>%
        summarise(steps = sum(steps, na.rm = TRUE))
hist(totalSteps$steps, xlab = "Steps", 
     main = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
meanOfSteps <- mean(totalSteps$steps)
medianOfSteps <- median(totalSteps$steps)
```

There are totally 2304 missing values in the dataset.  

`dataFilled` is a new dataset that equal to the original data with all NAs 
imputed with the mean for that 5-min interval. 

The mean and median total number of steps taken per day with imputed data are
10,766.19 and 
10,766.19 respectively. They are different from
the estimates from the first part of the assignment. Imputing missing data
 increased the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

The plot of activity patterns between weekdays and weekends is showed below. The 
x-axis showed the 5-minute intervals in one day, the y-axis showed the average 
number of steps taken at that 5-minute interval across all days.


```r
dataFilled$wday <- weekdays(dataFilled$date, abbreviate = TRUE)
for(i in seq(nrow(dataFilled))){
        if(dataFilled$wday[i] %in% c("Sun", "Sat")) {
                dataFilled$wday[i] <- "weekend"
        } else {
                dataFilled$wday[i] <- "weekday"
        }
}
dataFilled$wday <- as.factor(dataFilled$wday)

byInterval2 <- dataFilled %>%
        group_by(interval, wday) %>%
        summarise(steps = mean(steps, na.rm = TRUE))

library(ggplot2)
ggplot(data = byInterval2, aes(interval, steps)) +
        geom_line() +
        facet_wrap(wday~., nrow = 2) +
        labs(y = "number of steps", 
             title = "Activity patterns between weekdays and weekends")+
        theme_bw()
```

![](PA1_template_files/figure-html/differences-1.png)<!-- -->

The activity pattern in weekdays is different from that in weekend. There is a
significant peak at around 800th interval during weekdays but not during weekends.
