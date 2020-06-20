1. Loading and preprocessing of data
------------------------------------

    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

    fileZIP <- "./week2/activity.zip" 

    fileDIR <- "./week2"

    Unzip <- "./week2/data"  

    if (!file.exists(fileDIR)){
      dir.create(fileDIR)
    }

    download.file(fileURL,file.path(fileZIP))

    unzip(fileZIP,exdir=Unzip) 

    fileDATA <- file.path(Unzip,"activity.csv")

    Activity <- read.csv(fileDATA)

    summary(Activity)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

2. What is mean total number of steps taken per day?
----------------------------------------------------

### 2.1 Calculate the total number of steps taken per day

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    Activity_tbl <- tbl_df(Activity)

    GroupDate <- Activity_tbl %>% select(date, steps) %>% group_by(date) %>% summarize(Totalsteps= sum(steps)) %>% na.omit()

    hist(GroupDate$Totalsteps, xlab = "Total Steps by Day", main="Total Steps by Day", 
         ylim=c(0, 12), xlim = c(0, 25000), breaks = 20, border="white", 
         col="coral")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

### 2.2 Calculate and report the mean and median of the total number of steps taken per day

    mean(GroupDate$Totalsteps)

    ## [1] 10766.19

    median(GroupDate$Totalsteps)

    ## [1] 10765

3. What is the average daily activity pattern?
----------------------------------------------

### 3.1 Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    library(ggplot2)

    GroupInterval <- Activity_tbl %>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(Totalsteps= mean(steps)) 

    ggplot(GroupInterval, aes(x=interval, y=Totalsteps))+ geom_line() 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    GroupInterval[which(GroupInterval$Totalsteps== max(GroupInterval$Totalsteps)),]

    ## # A tibble: 1 x 2
    ##   interval Totalsteps
    ##      <int>      <dbl>
    ## 1      835       206.

4. Imputing missing values
--------------------------

### 4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    Activity_tbl %>% select(steps) %>% filter(is.na(steps)) %>% count(steps = "NA")

    ## # A tibble: 1 x 2
    ##   steps     n
    ##   <chr> <int>
    ## 1 NA     2304

### 4.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    ReplaceNAs  <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

    StepsMean <- (Activity_tbl %>% group_by(interval) %>% mutate(steps = ReplaceNAs(steps)))

    head(StepsMean)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [6]
    ##    steps date       interval
    ##    <dbl> <chr>         <int>
    ## 1 1.72   2012-10-01        0
    ## 2 0.340  2012-10-01        5
    ## 3 0.132  2012-10-01       10
    ## 4 0.151  2012-10-01       15
    ## 5 0.0755 2012-10-01       20
    ## 6 2.09   2012-10-01       25

### 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

    NewDataActivity <- StepsMean

    NewDataActivity

    ## # A tibble: 17,568 x 3
    ## # Groups:   interval [288]
    ##     steps date       interval
    ##     <dbl> <chr>         <int>
    ##  1 1.72   2012-10-01        0
    ##  2 0.340  2012-10-01        5
    ##  3 0.132  2012-10-01       10
    ##  4 0.151  2012-10-01       15
    ##  5 0.0755 2012-10-01       20
    ##  6 2.09   2012-10-01       25
    ##  7 0.528  2012-10-01       30
    ##  8 0.868  2012-10-01       35
    ##  9 0      2012-10-01       40
    ## 10 1.47   2012-10-01       45
    ## # … with 17,558 more rows

### 4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### Histogram of the total number of steps taken each day:

    GroupDate_2 <- NewDataActivity %>% select(date, steps) %>% group_by(date) %>% summarize(Totalsteps= sum(steps))

    ## Adding missing grouping variables: `interval`

    hist(GroupDate_2$Totalsteps, xlab = "Total Steps by Day", main="Total Steps by Day", 
         ylim=c(0, 20), xlim = c(0, 25000), breaks = 20, border="white", 
         col="coral")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

#### Comparison between mean and median of total number of steps taken per day in both datan set:

    Groupdate_mean <- mean(GroupDate$Totalsteps, na.rm = TRUE)

    Groupdate_2_mean <- mean(GroupDate_2$Totalsteps)

    Groupdate_mean 

    ## [1] 10766.19

    Groupdate_2_mean 

    ## [1] 10766.19

    Groupdate_median <- median(GroupDate$Totalsteps, na.rm = TRUE)

    Groupdate_2_median <- median(GroupDate_2$Totalsteps)

    Groupdate_median 

    ## [1] 10765

    Groupdate_2_median 

    ## [1] 10766.19

5. Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------

### 5.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    NewDataActivity$Weekend_Weekday <- ifelse(weekdays(as.Date(NewDataActivity$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

    head(NewDataActivity)

    ## # A tibble: 6 x 4
    ## # Groups:   interval [6]
    ##    steps date       interval Weekend_Weekday
    ##    <dbl> <chr>         <int> <chr>          
    ## 1 1.72   2012-10-01        0 Weekday        
    ## 2 0.340  2012-10-01        5 Weekday        
    ## 3 0.132  2012-10-01       10 Weekday        
    ## 4 0.151  2012-10-01       15 Weekday        
    ## 5 0.0755 2012-10-01       20 Weekday        
    ## 6 2.09   2012-10-01       25 Weekday

### 5.2 Make a panel plot containing a time series plot (type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    MainSteps<- NewDataActivity %>% group_by(interval, Weekend_Weekday) %>% summarise(Mean = mean(steps))


    head(MainSteps)

    ## # A tibble: 6 x 3
    ## # Groups:   interval [3]
    ##   interval Weekend_Weekday   Mean
    ##      <int> <chr>            <dbl>
    ## 1        0 Weekday         2.25  
    ## 2        0 Weekend         0.215 
    ## 3        5 Weekday         0.445 
    ## 4        5 Weekend         0.0425
    ## 5       10 Weekday         0.173 
    ## 6       10 Weekend         0.0165

    ggplot(MainSteps, mapping = aes(x = interval, y = Mean)) + geom_line() +
        facet_grid(Weekend_Weekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
