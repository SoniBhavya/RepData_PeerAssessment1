---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data<-read.csv("D:/Reproducible research/week2/RepData_PeerAssessment1/activity.csv",na.strings = "NA")
```

## What is mean total number of steps taken per day?

```r
data2<-data
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
data2.2<-group_by(data2,date)
data2.3<-summarise(data2.2,total_steps = sum(steps,na.rm = TRUE))

hist(data2.3$total_steps)
```

![](PA1_template_files/figure-html/step2-1.png)<!-- -->

```r
mean(data2.3$total_steps,na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(data2.3$total_steps,na.rm = TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

```r
data3<-data
library(dplyr)
data3.2<-group_by(data3,interval)
data3.3<-summarise(data3.2,mean_steps = mean(steps,na.rm = TRUE))
plot(data3.3$interval,data3.3$mean_steps,type = "l")
```

![](PA1_template_files/figure-html/step3-1.png)<!-- -->

```r
(filter(data3.3,mean_steps == max(data3.3$mean_steps)))$interval
```

```
## [1] 835
```

## Imputing missing values

```r
data4<-data
library(dplyr)
data4.2<-group_by(data4,interval)
data4.3<-summarise(data4.2,mean_steps = mean(steps,na.rm = TRUE))

sum(is.na(data4$steps))
```

```
## [1] 2304
```

```r
data4.4<-data4

getmean<-function(arg_interval)
{
  ans = filter(data4.3,interval == arg_interval)
  ans$mean_steps
}

for(i in 1:nrow(data4.4))
{
  if(is.na(data4.4[i,]$steps))
  {
    data4.4[i,]$steps = getmean(data4.4[i,]$interval)
  }
}

data4.5<-group_by(data4.4,date)
data4.6<-summarise(data4.5,total_steps = sum(steps,na.rm = TRUE))

hist(data4.6$total_steps)
```

![](PA1_template_files/figure-html/step4-1.png)<!-- -->

```r
mean(data4.6$total_steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(data4.6$total_steps,na.rm = TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
name<-c("Day")
data4.4[,name]<-NA

for(i in 1:nrow(data4.4))
{
  if(weekdays(as.Date(data4.4[i,]$date)) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
  {
    data4.4[i,]$Day = "Weekday"
  }
  else
  {
    data4.4[i,]$Day = "weekend"
  }
}


data4.7 <- data4.4 %>%
  group_by(Day, interval) %>%
  summarise(avg_steps = mean(steps))


xyplot(avg_steps~interval | as.factor(Day), data =  data4.7, type = "l", layout = c(1,2))
```

![](PA1_template_files/figure-html/step5-1.png)<!-- -->


