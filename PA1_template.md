# Reproducible Research: Peer Assessment 1

This is a simple analysis of the activity data collected by a subject during the months of October and November 2012. The data collected is the number of steps taken in 5 minute intervals each day.

The data is available [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations.

## Loading and preprocessing the data

The activity.csv file is extracted from the zipped file to the activity folder in your current working directory. 
 

```r
activity <- read.csv("activity/activity.csv",header=TRUE,stringsAsFactors = FALSE)
dim(activity)
```

```
## [1] 17568     3
```

The data read has 17568 observations and 3 columns.


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The format of the data is as required for the initial analysis.


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Also, a sample of the data is shown above.

## What is mean total number of steps taken per day?

Shown below is the histogram of the total number of steps taken for each day.


```r
library(plyr)
totStepsDay<- ddply(activity,.(date),summarize,daytotal = sum(steps))
with(totStepsDay,hist(daytotal,xlab="Total steps per day",main="Histogram of Total number of steps taken each day"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
with(totStepsDay,summary(daytotal))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

Ignoring the missing values, the mean of the total number of steps taken per day is 10770 steps. The median of the total number of steps taken per day is 10760.

**Note**: My analysis in R gave the numbers in the text (different from above).

## What is the average daily activity pattern?

Shown below is the histogram of mean of the total steps taken during each 5 minute interval across all the days.


```r
mnStepsInt <- ddply(activity,.(interval),summarize,intervalmean = mean(steps,na.rm=TRUE))
##dim(mnStepsInt)
##head(mnStepsInt)
with(mnStepsInt,plot(interval,intervalmean,type='l',main='Plot of average number of steps for each interval across all days',xlab='Interval',ylab='Mean of steps taken'))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


```r
mnStepsInt[which(mnStepsInt$intervalmean == max(mnStepsInt$intervalmean)),]
```

```
##     interval intervalmean
## 104      835        206.2
```

On average, the maximum number of steps the subject takes in a 5 minute interval each day is 206 at 8:35 AM.

## Imputing missing values

There are a total of 2304 observations with missing data.


```r
sum(is.na(activity))
```

```
## [1] 2304
```

The result below shows that all days in October and November are included in the data set.


```r
sum(is.na(activity$date))
```

```
## [1] 0
```

The dataset provided has no data available for 8 days. Those days are shown below.


```r
nadata <- activity[which(is.na(activity)),]
##dim(nadata)
table(nadata$date)
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

The 288 for each day is the same as the total number of 5 minute intervals in a 24 hour period.


```r
naindices <- which(is.na(activity))
##length(naindices)
noNAactivity <- activity
for(i in 1:length(naindices)){noNAactivity$steps[naindices[i]] <- as.integer(mnStepsInt$intervalmean[mnStepsInt$interval ==                                                                                               noNAactivity$interval[naindices[i]]])}
sum(is.na(noNAactivity))
```

```
## [1] 0
```

The noNAactivity copy of the original data has no missing values.


```r
noNAtotStepsDay <- ddply(noNAactivity,.(date),summarize,daytotal = sum(steps))
with(noNAtotStepsDay,hist(daytotal,xlab="Total steps per day",main="Histogram of Total number of steps taken each day with no missing data"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

The histogram above shows mean of the total steps taken during each 5 minute interval across all the days with no missing data.


```r
par(mfrow=c(1,2))
with(totStepsDay,hist(daytotal,xlab="Total steps per day",main="Original data"))
with(noNAtotStepsDay,hist(daytotal,xlab="Total steps per day",main="With no missing data"))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

The new histogram shows the increased counts because of imputing the missing values. 

Mean (10770) and median (10760) with missing data.


```r
summary(totStepsDay$daytotal)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

Mean (10750) and median (10640) with no missing data.

```r
summary(noNAtotStepsDay$daytotal)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10600   10700   12800   21200
```

Imputing the missing values decreases the mean and median values.

## Are there differences in activity patterns between weekdays and weekends?
To factor the days into weekday (Monday through Friday) and weekend (Saturday and Sunday), a new column is added to the data set (with no missing values) that gives the day of the week. 


```r
noNAactivity$day <- weekdays(as.Date(noNAactivity$date))
##dim(noNAactivity)
table(noNAactivity$day)
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##      2592      2592      2304      2304      2592      2592      2592
```

The rows with weekend days are identified.


```r
weekend <- which(noNAactivity$day %in% c("Saturday","Sunday"))
length(weekend)
```

```
## [1] 4608
```

A new column that shows whether it is a weekday or weekend is added. After coding the type of the day, the column is changed to a factor column.


```r
noNAactivity$daytype <- "Weekday"
for(i in 1:length(weekend)){
  noNAactivity$daytype[weekend[i]] <- "Weekend"}
noNAactivity$daytype <- as.factor(noNAactivity$daytype)
##dim(noNAactivity)
str(noNAactivity)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : int  1 0 0 0 0 2 0 0 0 1 ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Monday" "Monday" "Monday" "Monday" ...
##  $ daytype : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

The figure below shows the average number of steps taken for each 5 minute interval across weekdays and weekends.


```r
mnStepsIntDay <- ddply(noNAactivity,.(interval,daytype),summarize,intervalmean = mean(steps))
##dim(mnStepsIntDay)
##head(mnStepsIntDay)
library(lattice)
xyplot(intervalmean ~ interval | daytype, data = mnStepsIntDay, layout = c(1,2),type = 'l',xlab = "Interval",ylab = "Mean of steps taken",main = "Average number of steps for each interval across weekdays and weekends")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

We can see that on weekdays, the activity peaks at around 8:35 AM. There are three more peaks activity times as the day progresses. Also, the subject is more active early in the morning compared to the weekends. 

During the weekends, the subject is relatively more active. The activity is more evenly distributed across the day.
