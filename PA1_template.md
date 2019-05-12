---
title: "Repo_research_project1"
author: "Sathya Thiruvengadam"
date: "May 2, 2019"
output: 
    html_document:
        keep_md: true
---

## This the code for reproduciable research project 1



Loading needed package from library

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
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
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.5.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

### Q.1 Read csv file and store in the variable "dfile"


```r
dfile <- read.csv("./activity.csv")
```


### Q.2 Calculate total number of steps per day


```r
numofsteps <- tapply(dfile$steps, dfile$date, sum, na.rm=TRUE)
```

Plot a histogram


```r
qplot(numofsteps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/Numofsteps-1.png)<!-- -->

To calculate the mean and median of total number of steps per day

```r
summary(numofsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
mean_numofsteps <- mean(numofsteps)
median_numofsteps <- median(numofsteps)
```

### Q.3 Time series plot visualize the total number of steps, every day for an interval

```r
dfile %>% group_by(interval) %>%
    summarise(meanstepsperinterval = mean(steps, na.rm = TRUE)) %>%
    ggplot(aes(x=interval, y=meanstepsperinterval)) + geom_line() + labs(title = "Average steps per interval", x="Interval", y = "Avg steps") + theme(plot.title = element_text(hjust=0.5), plot.background = element_rect("aquamarine1"))
```

![](PA1_template_files/figure-html/Avgstepsperinterval-1.png)<!-- -->

To get the interval with maximum steps every day

```r
dfile %>% group_by(interval) %>%
    summarise(meanstepsperinterval = mean(steps, na.rm=TRUE)) %>%
    filter(meanstepsperinterval == max(meanstepsperinterval)) 
```

```
## # A tibble: 1 x 2
##   interval meanstepsperinterval
##      <int>                <dbl>
## 1      835                 206.
```

Total number of rows with missing value NA in the dataset

```r
colSums(is.na(dfile))
```

```
##    steps     date interval 
##     2304        0        0
```

### Q.4 Fill all NA's with column mean and create a new data frame "dfileNoNas" with missing data filled in.

```r
dfile %>% mutate_all(~ifelse(is.na(.x),mean(.x, na.rm=TRUE), .x)) -> dfileNoNAs
```

Plot a histogram for total number of steps every data with the Nas filled data

```r
TotalstepsdfileNoNAs <- aggregate(steps ~ date, data = dfileNoNAs, sum)
ggplot(TotalstepsdfileNoNAs, aes(x=steps)) + geom_histogram(binwidth = 500) +
    xlab("Total number of steps") +
    ggtitle("Total steps per day") +
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect("aquamarine1"))
```

![](PA1_template_files/figure-html/NumofstepsNoNAs-1.png)<!-- -->

Calculate mean and median of the edited dataset

```r
summary(TotalstepsdfileNoNAs$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
Totalstepsmean=mean(TotalstepsdfileNoNAs$steps)
Totalstepsmedian = median(TotalstepsdfileNoNAs$steps)
```

Difference in distribution before and after missing values got filled-
We can see the distribution is symmetric once the missing values are filled,
mean is equal to median, where it was negative skewed before with mean less than median.

### Q.5 Creating new variable DaysOfWeek using the function weekdays()

```r
dfileNoNAs$date <- as.Date(dfileNoNAs$date, origin=lubridate::origin)
weekdays1 <- c("Monday","Tuesday","Wednesday","Thursday","Firday")
dfileNoNAs$DaysOfWeek <- factor((weekdays(dfileNoNAs$date) %in% weekdays1), 
                        levels=c(FALSE, TRUE),labels=c('weekend',"weekday"))
```

Generate a plot, showing the average number of steps based on interval

```r
averageddfileNoNAs <- aggregate(steps ~ interval + DaysOfWeek, data=dfileNoNAs, mean)
ggplot(averageddfileNoNAs, aes(interval, steps)) + 
    geom_line() +
    facet_wrap(~DaysOfWeek,ncol=1) +
    labs(title="Steps per interval based on weekdays", x="Interval", y="Average number of steps")+ theme(plot.title = element_text(hjust=0.5), plot.background = element_rect("aquamarine1"))
```

![](PA1_template_files/figure-html/Totalstepsbydaysofweek-1.png)<!-- -->
