## Load the data
if (!file.exists("activity.csv")) {
    unzip("repdata-data-activity.zip")
}
activity <- read.csv("activity.csv")

## Clean the data
activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)

## What is mean total number of steps taken per day?
### (For this part of the assignment, you can ignore the missing values in the dataset.)
### Calculate the total number of steps taken per day
library(plyr)
steps_per_day <- ddply(activity, .(date), summarise, sum = sum(steps, na.rm=T))
steps_per_day

### Make a histogram of the total number of steps taken each day
hist(steps_per_day$sum, ylab="Number of Days", col="blue", xlab="Number of Steps", main="Histogram of Steps Per Day, Oct-Nov 2012")

### Calculate and report the mean of the total number of steps taken per day
mean(steps_per_day$sum)

### Calculate and report the median of the total number of steps taken per day
median(steps_per_day$sum)

## What is the average daily activity pattern?
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
### number of steps taken, averaged across all days (y-axis)
library(ggplot2)
steps_per_interval <- ddply(activity, .(interval), summarise, sum = sum(steps, na.rm=T))
p <- ggplot(steps_per_interval, aes(x=interval, y=sum, group=1)) 
p + geom_line() + labs(title = "Average Steps per Day by 5-min Intervals, Oct-Nov 2012") + labs(x = "5-minute Intervals", y = "Average Number of Steps")

### Which 5-minute interval, on average across all the days in the dataset, contains the 
### maximum number of steps?
steps_per_interval[ which(steps_per_interval$sum==(max(steps_per_interval$sum))), ]

## Imputing missing values
### Note that there are a number of days/intervals where there are missing values (coded as NA). 
### The presence of missing days may introduce bias into some calculations or summaries of the data.
### Calculate and report the total number of missing values in the dataset 
### (i.e. the total number of rows with NAs)
NA_values <- activity[!complete.cases(activity),]
nrow(NA_values)

### Devise a strategy for filling in all of the missing values in the dataset. 
### The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
### or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

### Calculate the mean for each interval
interval_mean <- ddply(activity, .(interval), summarise, mean = mean(steps, na.rm=T))
interval_mean
### Add the interval mean as a new variable to the activity dataset 
activity_with_interval_mean <- join(activity, interval_mean)
### Write function that will replace NA values with the interval mean
replace_NA <- function(dataset, variable, replacement) {
    for (i in 1:nrow(dataset)) {
        if (is.na(dataset[i, variable])) {
                dataset[i, variable] <- dataset[i, replacement]
        }
    }
    dataset
}

### Run the function on the dataset
complete_activity <- replace_NA(dataset=activity_with_interval_mean, variable=1, replacement=4)
complete_activity <- complete_activity[, -4]
head(complete_activity)

### Make a histogram of the total number of steps taken each day and calculate and report the mean and 
### median total number of steps taken per day. 
complete_steps_per_day <- ddply(complete_activity, .(date), summarise, sum = sum(steps))
complete_steps_per_day$sum <- round(complete_steps_per_day$sum)
hist(complete_steps_per_day$sum, ylab="Number of Days", col="red", xlab="Number of Steps", main="Histogram of Steps Per Day, Oct-Nov 2012")

### Calculate and report the mean of the total number of steps taken per day with imputed NAs
mean(complete_steps_per_day$sum)

### Calculate and report the median of the total number of steps taken per day with imputed NAs
median(complete_steps_per_day$sum)

### Do these values differ from the estimates from the first part of the assignment? 
### Mean comparison
mean(steps_per_day$sum)
mean(complete_steps_per_day$sum)

### Median comparison
median(steps_per_day$sum)
median(complete_steps_per_day$sum)

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### Separate histograms
par(mfrow=c(1,2)) 
hist(steps_per_day$sum, ylab="Number of Days", col=rgb(1,0,0,0.2), xlab="Number of Steps", main="Steps Per Day, Excl. NA")
hist(complete_steps_per_day$sum, ylab="Number of Days", col=rgb(0,0,1,0.2), xlab="Number of Steps", main="Steps Per Day, Impute NA")

### Overlapping histograms
hist(steps_per_day$sum,ylim=c(0,25),breaks=10,col=rgb(1,0,0,0.2), ylab="Number of Days", xlab="Number of Steps", main="Steps Per Day, NA vs Imputed")
par(new=TRUE)
hist(complete_steps_per_day$sum,ylim=c(0,25),breaks=10,col=rgb(0,0,1,0.2),main="",xlab="",ylab="")
legend('topleft',c('NA','Imputed'),
       fill = c(rgb(1,0,0,0.2), rgb(0,0,1,0.2)), bty = 'n',
       border = NA)


## Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here. 
### Use the dataset with the filled-in missing values for this part.
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend”,
### indicating whether a given date is a weekday or weekend day.
library(timeDate)
complete_activity$day_of_week <- ifelse(isWeekday(complete_steps_per_day$date)==TRUE, "weekday", "weekend")

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
library(lattice)
xyplot(steps ~ interval | day_of_week, layout = c(1, 2), data=complete_activity, type="l")

library(knitr)
knit2html()


