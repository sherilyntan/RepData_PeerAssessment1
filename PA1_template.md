Loading and preprocessing the data

    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(data.table)

    ## -------------------------------------------------------------------------

    ## data.table + dplyr code now lives in dtplyr.
    ## Please library(dtplyr)!

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    library (lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(knitr)
    setwd("c:/coursera")
    activity <- read.csv("activity.csv")

Make a histogram on total number of steps per day

    aggsteps<-aggregate(steps~date, data=activity, FUN = sum)
    hist(aggsteps$steps,xlab = "Number of steps", main = "Total Number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

Calculate and report the mean and median total number of steps taken per
day

mean

    mean(aggsteps$steps)

    ## [1] 10766.19

median

    median(aggsteps$steps)

    ## [1] 10765

What is the average daily activity pattern? Make a time series plot
(i.e.type = "l") of the 5-minute interval (x-axis) and the average
number of steps taken, averaged across all days (y-axis)

    averages <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), 
                          FUN = mean, na.rm = TRUE)
    ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
            ylab("average number of steps taken")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    averages[which.max(averages$steps), ]

    ##     interval    steps
    ## 104      835 206.1698

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs).

    na<-is.na(activity$steps)
    table(na)

    ## na
    ## FALSE  TRUE 
    ## 15264  2304

Inputing missing values,NA, for days/intervals

    fill.value <- function(steps, interval) {
            filled <- NA
            if (!is.na(steps)) 
                    filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
                    return(filled)
    }
    filled.data <- activity
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

Make a histogram of the total number of steps taken each day and
calculate the mean and median total number of steps.

    total.steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
    qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and
weekends?

    dayweek<-function(date){
            day<-weekdays(date)
            if(day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
                    return("weekday") else if (day %in% c("Saturday","Sunday"))
                            return("weekend") else stop ("invalid date")}
            
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN = dayweek) 

Make a panel plot containing plots of average number of steps taken on
weekdays and weekends.

    averages<-aggregate(steps~interval + day, data = filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
            xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
