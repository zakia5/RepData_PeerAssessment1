---
title: "Reproducible Research: Peer Assessment 1"
author: "Zakia Sultana"
date: "March 3, 2016"
output: html_document
---
Loading necessary libraries
---

```{r}
library(ggplot2)
library(lubridate)
library(dplyr)
```
Loading and preprocessing the data
---
```{r}
activity <- read.csv(file= "activity.csv", head=TRUE, sep=",")
str(activity) # View data structure
activity$date <- ymd(activity$date)

summary(activity) # View data Summary
```
Question one : what is mean total number of steps taken per day?
---
For this part of the work, we ignore the missing values in the dataset.

Step 1) Calculating the total number of steps taken per day:

```{r}
totalstepsperday=aggregate(steps~date,activity,sum) # sum total steps over each day
head(totalstepsperday)
hist(totalstepsperday$steps,  # Frequency(histogram) of total steps per day
     col="lightblue",
     border="blue4",
     lty=2,
     main="Histogram Of Total Number Of Steps Per Day",
     xlab="Total Number Of Steps Per Day")
mtext("(With Missing Values)")
```


```{r}
meantotsteps=mean(totalstepsperday$steps) # Average total steps per day
mediantotsteps=median(totalstepsperday$steps)# Median total steps per day 
```
Mean Total Number Of Steps Per Day (with missing values) : 10766.19

Median Total Number Of Steps Per Day (with missing values) : 10765

Question two: what is the average daily activity pattern?
---
Generate New Dataframe to track average steps per interval :


```{r}
avgstepsperinterval=aggregate(steps~interval,activity,mean) # avg step per interval
str(avgstepsperinterval);summary(avgstepsperinterval)
```

Time Series Plot - Average Steps per 5 min interval Over all Days

```{r}
par(mar=c(5,6,4,2))
ggplot(avgstepsperinterval, aes(interval, steps)) + geom_line() + ggtitle("Avg Steps Per 5 min Interval Over All Day")
```

Interval with Maximum Average Steps :

```{r}
intmaxsteps=avgstepsperinterval$interval[avgstepsperinterval$steps==max(avgstepsperinterval$steps)]
```
5 min Interval with Maximum Average Steps : 835


Question three: imputing missing values
---
Number of Missing Values :

```{r}
numissingvalues=nrow(activity[is.na(activity$steps),])
numissingvalues
```
Total Number of Missing Values : 2304

Filling Missing Values (use mean of time interval) :

```{r}
activity1=merge(activity,avgstepsperinterval,by="interval") # merge
summary(activity1) # Review data frame & NA totals
```

```{r}
# replace NA's with mean of intervals
activity1$steps.x[is.na(activity1$steps.x)]=round(activity1$steps.y[is.na(activity1$steps.x)],0) 
activity1=activity1[,c('interval','steps.x','date')] # Drop the merged column
names(activity1)[2]='steps' # rename steps.x to steps
summary(activity1) # re-check values for NA's
```
** Note absence of NA's in the second summary above

Histogram, mean and median of total steps taken per day for New Dataframe equal to activity, but with missing values filled :

```{r}
newtotalstepsperday=aggregate(steps~date,activity1,sum) # sum total steps over each day
hist(newtotalstepsperday$steps,col="green",lty=2,main="Histogram Of Total Number Of Steps Per Day",
     xlab="Total Number Of Steps Per Day")
mtext("(Missing Values Filled)")
```

Average and Median total steps per day

```{r}
newmeantotsteps=mean(newtotalstepsperday$steps) 
newmediantotsteps=median(newtotalstepsperday$steps)
```
New Mean Total Number Of Steps Per Day (Missing values filled) : 10765.64

New Median Total Number Of Steps Per Day (Missing values filled) : 10762

Impact Of Adding Missing Values :

```{r}
old=c("mean"=meantotsteps,"median"=mediantotsteps)
new=c("mean"=newmeantotsteps,"median"=newmediantotsteps)
oldnew=data.frame(old,new)
oldnew$diff=(new-old)/old*100
oldnew
```
"diff" column in the above dataframe indicates the % difference in the mean and median values from the earlier estimates with missing values and current estimates with missing values filled in. There is a very marginal, practically negligible difference between the earlier and current estimates.
Question four: are there differences in activity patterns between weekdays and weekends?
---
add a day column for weekdays/weekends and weekday function to identify weekends

```{r}
activity1$day[weekdays(as.Date(activity1$date))%in%c("Sunday","Saturday")]="weekend" 
activity1$day[is.na(activity1$day)]="weekday" # All other days are weekdays
activity1$day=as.factor(activity1$day) # convert this to factor variable
str(activity1) 
```
Find average steps per interval by type of day
```{r}
average=aggregate(steps~interval+day,activity1,mean)  
```
```{r}
library(lattice)
xyplot(steps~interval|day,
       average,
       type="l",
       main="Avg Steps Per 5 min Interval Over All Days",
       xlab="Time Intervals",
       ylab="Avg Steps Per 5 min Interval \nOver All Days",
       col="blue",
       layout=c(1,2))
```
