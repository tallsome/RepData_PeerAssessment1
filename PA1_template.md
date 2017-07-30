# Reproducible Research: Peer Assessment 1




I loaded the dplyr and lattice library for analysis.

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


## Loading and preprocessing the data


```r
#1. Reading the data into a dataframe
df<-read.csv("activity.csv", header=TRUE)

#Removes missing value from original dataframe
df1<-filter(df, !is.na(steps))
```

## What is mean total number of steps taken per day?


```r
#Sorting and adding total steps taken each day. 
stepperday<-group_by(df1,date)
totalstep<-summarise(stepperday, totalstepperday=sum(steps))
#2. Using base plot to generate histograme of mean total # of steps taken per day.
with(totalstep, hist(totalstepperday,xlab="Total Steps Per Day", main="Histogram of Total Steps Taken Each Day", col="blue"))
```

![](PA1_template_files/figure-html/average_steps-1.png)<!-- -->

```r
#3. Calculating the mean and median of mean total number of steps taken per day
mean(totalstep$totalstepperday)
```

```
## [1] 10766.19
```

```r
median(totalstep$totalstepperday)
```

```
## [1] 10765
```

The mean is 1.076619\times 10^{4} and median is 10765 for total number of steps taken per day.

## What is the average daily activity pattern?


```r
dailyactivity<-aggregate(steps~interval, data=df, mean, na.rm=T)
names(dailyactivity)<-c("interval", "avg_step")

#4. Time series plot of the average number of steps taken
with(dailyactivity, plot(x=interval, y=avg_step, type="l", col="blue", ylab="Avg Steps Taken, Averaged Across all Days", main="Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/dailyactivity-1.png)<!-- -->

```r
#5. The 5-minute interval that, on average, contains the maximum number of steps
maxinterval<-dailyactivity[which.max(dailyactivity$avg_step), ]$interval
```

The 5-minute interval that, on average, contains the maximum number of steps is 835.

## Imputing missing values

```r
#6. Code to describe and show a strategy for imputing missing data
#counting the total number of missing step values.
sum(is.na(df))
```

```
## [1] 2304
```

```r
#Merging the original dataframe with the dataframes for averages of the 5 minute intervals.
combineddf<-merge(dailyactivity, df)
#Subsetting only the NA rows and for steps and replacing them with the averages for that 5 #minute interval.  Placing the new and completed data into a new column labeled "step"
combineddf$step[is.na(combineddf$steps)]<-combineddf$avg_step[is.na(combineddf$steps)]

imputedstepperday<-combineddf%>% group_by(date) %>% summarise(totalstepperday= sum(step))
#7. Histogram of the total number of steps taken each day after missing values are imputed
with(imputedstepperday, hist(totalstepperday,xlab="Total Steps Per Day", main="Histogram of Total Steps Taken Each Day (Imputed)", col="green"))
```

![](PA1_template_files/figure-html/imputedData-1.png)<!-- -->

```r
mean(imputedstepperday$totalstepperday)
```

```
## [1] 10766.19
```

```r
median(imputedstepperday$totalstepperday)
```

```
## [1] 10766.19
```

The mean is 1.076619\times 10^{4} and median is 1.0766189\times 10^{4} for total number of steps taken per day after imputing the missing data.

The values did not differ from the estimates before imputing the missing data, because the averages were used to fillin the missing spots.  There's no impact. 

## Are there differences in activity patterns between weekdays and weekends?

```r
#Added two additional columns "day" and "day_type" to define the day of the week then
#to categorize which is weekend and weekday using the ifelse statement.
combineddf$day<-weekdays(as.Date(combineddf$date))
combineddf$day_type<-ifelse (combineddf$day %in% c("Saturday", "Sunday"), "weekend", "weekday")

weekactivity<-combineddf%>% group_by(interval, day_type) %>% summarise(avg_stepperday= mean(step))
#8.  Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
with(weekactivity, xyplot(avg_stepperday ~ interval | factor(day_type), layout = c(1,2), type = "l", col="blue"))
```

![](PA1_template_files/figure-html/WeekPatterns-1.png)<!-- -->
