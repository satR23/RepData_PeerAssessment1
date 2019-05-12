This the code for reproduciable research project 1
--------------------------------------------------

Loading needed package from library

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)

    ## Warning: package 'lubridate' was built under R version 3.5.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

### Q.1 Read csv file and store in the variable "dfile"

    dfile <- read.csv("./activity.csv")

### Q.2 Calculate total number of steps per day

    numofsteps <- tapply(dfile$steps, dfile$date, sum, na.rm=TRUE)

Plot a histogram

    qplot(numofsteps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

![](Repo_res_project1_rmarkdown_files/figure-markdown_strict/Numofsteps-1.png)

To calculate the mean and median of total number of steps per day

    summary(numofsteps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10395    9354   12811   21194

    mean_numofsteps <- mean(numofsteps)
    median_numofsteps <- median(numofsteps)

### Q.3 Time series plot visualize the total number of steps, every day for an interval

    dfile %>% group_by(interval) %>%
        summarise(meanstepsperinterval = mean(steps, na.rm = TRUE)) %>%
        ggplot(aes(x=interval, y=meanstepsperinterval)) + geom_line() + labs(title = "Average steps per interval", x="Interval", y = "Avg steps") + theme(plot.title = element_text(hjust=0.5), plot.background = element_rect("aquamarine1"))

![](Repo_res_project1_rmarkdown_files/figure-markdown_strict/Avgstepsperinterval-1.png)

To get the interval with maximum steps every day

    dfile %>% group_by(interval) %>%
        summarise(meanstepsperinterval = mean(steps, na.rm=TRUE)) %>%
        filter(meanstepsperinterval == max(meanstepsperinterval)) 

    ## # A tibble: 1 x 2
    ##   interval meanstepsperinterval
    ##      <int>                <dbl>
    ## 1      835                 206.

Total number of rows with missing value NA in the dataset

    colSums(is.na(dfile))

    ##    steps     date interval 
    ##     2304        0        0

### Q.4 Fill all NA's with column mean and create a new data frame "dfileNoNas" with missing data filled in.

    dfile %>% mutate_all(~ifelse(is.na(.x),mean(.x, na.rm=TRUE), .x)) -> dfileNoNAs

Plot a histogram for total number of steps every data with the Nas
filled data

    TotalstepsdfileNoNAs <- aggregate(steps ~ date, data = dfileNoNAs, sum)
    ggplot(TotalstepsdfileNoNAs, aes(x=steps)) + geom_histogram(binwidth = 500) +
        xlab("Total number of steps") +
        ggtitle("Total steps per day") +
        theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect("aquamarine1"))

![](Repo_res_project1_rmarkdown_files/figure-markdown_strict/NumofstepsNoNAs-1.png)

Calculate mean and median of the edited dataset

    summary(TotalstepsdfileNoNAs$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

    Totalstepsmean=mean(TotalstepsdfileNoNAs$steps)
    Totalstepsmedian = median(TotalstepsdfileNoNAs$steps)

Difference in distribution before and after missing values got filled-
We can see the distribution is symmetric once the missing values are
filled, mean is equal to median, where it was negative skewed before
with mean less than median.

### Q.5 Creating new variable DaysOfWeek using the function weekdays()

    dfileNoNAs$date <- as.Date(dfileNoNAs$date, origin=lubridate::origin)
    weekdays1 <- c("Monday","Tuesday","Wednesday","Thursday","Firday")
    dfileNoNAs$DaysOfWeek <- factor((weekdays(dfileNoNAs$date) %in% weekdays1), 
                            levels=c(FALSE, TRUE),labels=c('weekend',"weekday"))

Generate a plot, showing the average number of steps based on interval

    averageddfileNoNAs <- aggregate(steps ~ interval + DaysOfWeek, data=dfileNoNAs, mean)
    ggplot(averageddfileNoNAs, aes(interval, steps)) + 
        geom_line() +
        facet_wrap(~DaysOfWeek,ncol=1) +
        labs(title="Steps per interval based on weekdays", x="Interval", y="Average number of steps")+ theme(plot.title = element_text(hjust=0.5), plot.background = element_rect("aquamarine1"))

![](Repo_res_project1_rmarkdown_files/figure-markdown_strict/Totalstepsbydaysofweek-1.png)