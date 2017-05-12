library(dplyr)
library(ggplot2)
options(digits=2)

getwd()
setwd("./Course5")

#read in data
activity <- read.csv("activity.csv", sep =",", stringsAsFactors = FALSE, na.strings = "NA")

#convert date to Date class
activity$date <- as.Date(activity$date)

#Part 1 - What is mean total number of steps taken per day?
#Use complete cases only
activityComplete <- activity[complete.cases(activity),]

#Aggregate by day
totalStepsByDay <- aggregate(steps ~ date, data = activityComplete, FUN=sum)
head(totalStepsByDay)
#mean and median of all days
mean(totalStepsByDay$steps)
median(totalStepsByDay$steps)

#create histogram of total steps by day
p1 <- ggplot(data = totalStepsByDay, aes(totalStepsByDay$steps)) + 
  geom_histogram(bins = 20, fill = "green", col="black") +
  labs(x = "Total Steps") +
  labs(y = "Frequency") +
  ylim(c(0,10)) +
  labs(title = "Total Steps per Day (20 bins)")
ggsave(file = "Plot1.png")

##Part 2 - What is the average daily activity pattern?
##Group by interval and calculate mean, and max steps
byInterval <- group_by(activityComplete, interval )
meanByInterval <- as.data.frame(summarize(byInterval, mean = mean(steps)))
maxStepsData <- subset(meanByInterval, meanByInterval$mean==max(meanByInterval$mean))

##Plot the average value of the intervals and highlight average and max values 
png('plot1.1.png')
plot(meanByInterval, type = "l", xlab = "Interval", ylab = "Steps Taken", main="Average steps taken by interval")
abline(h=mean(activityComplete$steps), col = "red")
points(maxStepsData$interval[], maxStepsData$mean[], col="red")
dev.off()


#the maximum interval and steps shown by the point above is:
maxStepsData$interval
maxStepsData$mean

##Part 3 - imputing missing values
#Function returnMean - Input: missing interval value. Output:matching mean steps from interval across dataset
returnMean <- function(interval){
  #lookup and return valid mean value
  meanByInterval[match(interval, meanByInterval$interval),2]
}

#loop through all the NA values and assign based on interval
activityImputed <- activity
for (index in 1:nrow(activityImputed)) { 
  rowData = activityImputed[index, ]
  #if steps is NA then impute new value by interval
  if (is.na(rowData$steps)) {
    rowData$steps <- returnMean(rowData$interval)
    activityImputed[index, ] <- rowData
  }
}

#aggregate by day with imputed values
totalStepsByDayImputed <- aggregate(steps ~ date, data = activityImputed, FUN=sum)
#mean and median of all days imputed 
mean(totalStepsByDayImputed$steps)
median(totalStepsByDayImputed$steps)


#this may seem incorrect at first but further analysis reveals that 8 days are completely missing.
aggregate(interval ~ date, data = subset(activity, is.na(activity$steps)==TRUE), FUN=length)
#thus calculation by average interval impution is going to assing all those days to the mean value and skew everything center

p2 <- ggplot(data = totalStepsByDayImputed, aes(totalStepsByDayImputed$steps)) + 
  geom_histogram(bins = 20, fill = "green", col="black") +
  labs(x = "Total Steps") +
  labs(y = "Frequency") +
  ylim(c(0,15)) +
  labs(title = "Total Steps per Day Imputed (20 bins)")
ggsave(file = "Plot2.png")



##Part 4 - Weekday/weekend activity patterns
#Are there differences in activity patterns between weekdays and weekends?
##modified from: http://stackoverflow.com/questions/28893193/creating-factor-variables-weekend-and-weekday-from-date
##categorize imputed activities by weekday/weekend
activityImputed$day <- factor(weekdays(activityImputed$date) %in% c("Saturday", "Sunday"),
                                      levels= c(TRUE, FALSE),
                                      labels= c('Weekend', 'Weekday'))


p3 <- ggplot(activityImputed, aes(interval,steps)) + 
  geom_line(color="red") +
  facet_grid(day ~ .)
ggsave(file = "Plot3.png")


library(rmarkdown)
render("PA1_Template.rmd")
