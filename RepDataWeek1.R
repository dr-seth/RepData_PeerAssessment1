### Load required packages

library(knitr)

# Load and preprocess the data
### Store the URL for data
```{r, echo = TRUE}
fileURL <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip")
```
### Store filenames and paths as varriables
```{r, echo = TRUE}
fileName <- "~/GitHub/RepData_PeerAssessment1/activity.csv"
compressedFileName <- "~/GitHub/RepData_PeerAssessment1/repdata_2Fdata_2Factivity.zip"
```
### Check to see if file exists. Download and extract only if it does not
```{r, echo = TRUE}
if (!file.exists(fileName)) {
  download.file(fileURL, destfile = zipFileName, method = "curl")
  unzip(zipFileName)
  date = Sys.Date()
}
```
# Create data frame in varriable 'activity'. data frame contains threee columns: 

```{r, echo = TRUE}
library(data.table)
activity <- fread(fileName, na.strings = "NA")
activity$date <- as.Date( activity$date, format = '%Y-%m-%d' )
```
# What is mean total number of steps taken per day?
### Calculate the sum of steps taken by day
```{r, echo = TRUE}
totalSteps <- aggregate(steps ~ date, data = activity,  sum)
totalSteps
```
### Create a histogram of total steps per day
```{r, echo = TRUE}
hist(totalSteps$steps, col = 'green',  xlab = 'Total Steps per Day')
dev.off()
```
### Calculate the mean and median of the total number of steps taken per day 
```{r, echo = TRUE}
meanSteps <- mean(totalSteps$steps)
medianSteps <- median(totalSteps$steps)
```
### Report the mean and median of the total number of steps taken per day 
```{r, echo = TRUE}
totalSteps
meanSteps
medianSteps
```
# What is the average daily activity pattern?
### Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axisand the average number of steps taken, averaged across all days (y-axis)
```{r, echo = TRUE}
fiveMinAve <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = fiveMinAve$interval, y = fiveMinAve$steps, type = "l")
dev.off()
```
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo = TRUE}
maxSteps <- max(fiveMinAve$steps)
for (i in 1:288) 
  {
       if (fiveMinAve$steps[i] == maxSteps)
              fiveMinIntMax <- fiveMinAve$interval[i]
      }
fiveMinIntMax
```
# Imputing missing values
### Calculate and report the total number of missing values inthe dataset
```{r, echo = TRUE}
steps <- activity$steps
countMissing <- length(steps)
```
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo = TRUE}
imputedSteps <- activity 
for (i in 1:nrow(imputedSteps)) {
  if (is.na(imputedSteps$steps[i])) {
    imputedSteps$steps[i] <- fiveMinAve[which(imputedSteps$interval[i] == fiveMinAve$interval), ]$steps
  }
}
```
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo = TRUE}
hist(imputedSteps$steps, col = 'green',  xlab = 'Total Steps per Day')
dev.off()
meanStepsImputed <- mean(imputedSteps$steps)
medianStepsImputed <- median(imputedSteps$steps)
```
# Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r, echo = TRUE}
library(plyr)
activityDOW <- mutate(imputedSteps, daytype = ifelse(weekdays(imputedSteps$date) 
          == "Saturday" | weekdays(imputedSteps$date) == "Sunday", "weekend", "weekday"))
head(activityDOW)
```
### Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r, echo = TRUE}
dayFiveMinAve <- aggregate(steps~interval + daytype, activityDOW, mean)
library(lattice)
xyplot(dayFiveMinAve$steps ~ dayFiveMinAve$interval | dayFiveMinAve$daytype, 
       main="Average Steps per Day by Interval", xlab="5 Second Interval", ylab="Average Steps per Day", 
      layout=c(1,2), type="l", cex=1, cex.axis=0.75, font=2, font.lab=2, font.main=2, font.sub=2, font.lab=2)
```