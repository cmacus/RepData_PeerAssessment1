---
title: "PA1"
output: html_document
---



## Histogram of steps taken per day


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
df <- read.csv('activity.csv')
dfc <- df[complete.cases(df),]
dfs <- aggregate(dfc$steps, by=list(date=dfc$date), FUN=sum)
dfs <- dfs[complete.cases(dfs),]
dfs$x <- as.numeric(as.character(dfs$x))
hist(dfs$x,main="Histogram of steps per day", xlab='Steps')
```

<img src="PA1_template_files/figure-html/steps_histogram-1.png" width="672" />

Mean 1.0766189\times 10^{4} and median 1.0765\times 10^{4}

## Time series plot for intervals


```r
int_s <- aggregate(dfc$steps, by=list(interval=dfc$interval),FUN=mean)
plot(int_s,type='l',main="Plot of intervals and steps")
```

<img src="PA1_template_files/figure-html/time_series-1.png" width="672" />

```r
i <- int_s[int_s$x == max(int_s$x),]
```

The interval 835 contains maximum steps

## Filling up NA rows


```r
dff <- df
i <- 1
for (i in 1:nrow(dff)) {
  row <- dff[i,]
  if (is.na(row$steps))
  {
    int_row <- row$interval
    mean_int = int_s[int_s$interval == int_row,2]
    dff[i,1] <- mean_int
    
  }
}
```

## Plotting histogram after fillup


```r
dfs <- aggregate(dff$steps, by=list(date=dff$date), FUN=sum)
dfs <- dfs[complete.cases(dfs),]
dfs$x <- as.numeric(as.character(dfs$x))
hist(dfs$x,main="Histogram of steps per day (after filling NA data)",
     xlab='Steps')
```

<img src="PA1_template_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## Weekdays and weekends

```r
weekends <- c("Saturday","Sunday")
dff$day_of_week <- 'weekday'
for (i in 1:nrow(dff))
{
  row_date <- dff[i,]$date

  day_of_week <- weekdays(strptime(row_date,'%Y-%m-%d'))
  if (day_of_week %in% weekends)
  {
    dff[i,]$day_of_week <- 'weekend'
  }
  else
    dff[i,]$day_of_week <- 'weekday'
}
dff$day_of_week <- as.factor(dff$day_of_week)
```

## Time series plot for intervals (weekdays and weekends)


```r
par(mfrow=c(2,1))
dff_wd <- dff[dff$day_of_week=='weekday',]
int_s_wd <- aggregate(dff_wd$steps, by=list(interval=dff_wd$interval),FUN=mean)
plot(int_s_wd,type='l',main="Plot of intervals and steps on weekdays")
i <- int_s_wd[int_s_wd$x == max(int_s_wd$x),]


dff_we <- dff[dff$day_of_week=='weekend',]
int_s_we <- aggregate(dff_we$steps, by=list(interval=dff_we$interval),FUN=mean)
plot(int_s_we,type='l',main="Plot of intervals and steps on weekends")
```

<img src="PA1_template_files/figure-html/time_series_weeday_weekends-1.png" width="672" />

```r
i <- int_s_we[int_s_we$x == max(int_s_we$x),]
```
