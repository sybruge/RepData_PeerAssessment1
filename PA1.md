# Pear Assessment 1 for reproducible Research
S. Brugere  
10 mai 2017  


  
## Loading and preprocessing the data

```r
if (! file.exists("activity.csv")) unzip("activity.zip")
data <- read.csv("activity.csv", header=T, sep=",")
```
  
  
## What is mean total number of steps taken per day?

```r
par(mfrow=c(1,1))
# sum of steps by date and histogram
total.by.day <- aggregate(steps~date,data=data,FUN="sum")
with(total.by.day, hist(steps, breaks=12))
# report mean and median on histogram with vertical blue and red lines
abline(v = mean(total.by.day$steps, na.rm=T), col = "blue", lwd=2)
abline(v = median(total.by.day$steps), col ="red", lwd=2, lty=2)
```

![](PA1_files/figure-html/histogram 1-1.png)<!-- -->
  
The mean total number of steps taken per day is **10766**.  
The median total number of steps taken per day is **10765**.  
  
## What is the average daily activity pattern?

```r
# calculate average of steps by interval
mean.by.interval <- aggregate(steps~interval, data = data, FUN = "mean", na.rm = TRUE)
# plot of type "l" : average by interval
plot(steps~interval, data = mean.by.interval, type="l", main="Average number of steps by interval")
# report the max average on the plot with vertical red line
abline(v = mean.by.interval[which.max(mean.by.interval$steps),1], col = "red", lwd = 2)
```

![](PA1_files/figure-html/plot 1-1.png)<!-- -->
  
The interval which corresponds to the max average number of steps is **835**.  
    
## Imputing missing values

```r
cnt <- sum(is.na(data$steps))
```
The number of missing values is **2304**.  


```r
# imputing steps column with mean of the interval when NA value found
data2 <- transform(data, steps = ifelse(is.na(steps), mean.by.interval[,2], data[,1]))
# sum of steps by date and histogram
total.by.day <- aggregate(steps~date, data=data2, FUN="sum")
with(total.by.day, hist(steps,12))
# report mean and median on histogram with blue and red vertical line
abline(v = mean(total.by.day$steps, na.rm=T), col = "blue", lwd=2)
abline(v = median(total.by.day$steps), col ="red", lwd=2, lty=2)
```

![](PA1_files/figure-html/histogram 2 (imputing NA)-1.png)<!-- -->
**The strategy with missing values is to replace NA values with the average of the interval.**  
**The only difference with the first histogram is the frequency of the interval of the mean value.**  
  
## Are there differences in activity patterns between weekdays and weekends?

```r
# calculate new column typeOfDay with weekdays (week day or week end)
data3 <- transform(data, typeOfDay = 
    ifelse(weekdays(as.Date(data$date,"%Y-%m-%d")) %in% c("samedi","dimanche"), "weekend", "weekday"))
# mean of steps by date and histogram
mean.by.interval <- aggregate(steps~interval+typeOfDay, data = data3, FUN = "mean", na.rm = TRUE)
# 2 facets plot  : steps function of interval for each type of day
qplot(x=interval, y=steps, data=mean.by.interval, geom="line", facets=typeOfDay~., main="Average number of steps by interval")
```

![](PA1_files/figure-html/plot 2-1.png)<!-- -->
