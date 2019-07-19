---
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
---
library(plyr)
library(timeDate)

#Read the data into Rstudio

data <- read.csv("activity.csv")
head(activity) #Take a look at the data

#The file with the NA's removed

df <-na.omit(activity)

#In the end the following file will hold the corrected data

d2 <- activity 

x <- ddply(df, .(as.Date(date)), summarize, freq=length(steps), tot=sum(steps), xmn = mean(steps), xmed = median(steps))
xbar <-mean(x$tot/288, na.rm = TRUE) 

#Calculate the average and median number of steps taken daily

x_bar <- mean(x$tot) 
x_med <- median(x$tot)
d2[is.na(d2)] <- xbar 
hist(x$tot, breaks = 10, main= "Histogram of Total Steps taken each Day", xlab = "Total Steps")

print(paste("The mean is",x_bar))
print(paste("The median is",x_med))

xx <- ddply(df, .(interval), summarize, freq=length(steps), tot=sum(steps)/freq)
wrk <- xx[,-2]
plot(wrk,type ="l", main = "Average Number of steps across all days")

ind <- which.max(xx$tot) #the index of the max value

# Find the number of rows with NA's

g <- activity[!complete.cases(activity),] 

xfix <- ddply(d2, .(as.Date(date)), summarize, freq=length(steps), tot=sum(steps), xmn = mean(steps), xmed = median(steps))
hist(xfix$tot, breaks = 10, main= "Histogram of Total Steps taken each Day", xlab = "Total Steps")

x_bar <- mean(xfix$tot)
x_med <- median(xfix$tot)

#Next is the average number of the steps based on weekdays or weekends

d2$day <- isWeekday(d2$date)
d2$state <- ifelse(d2$day =="TRUE", "Weekday","Weekend")
workday <- d2[which (d2$state == "Weekday"),]
offday <- d2[which (d2$state == "Weekend"),]
wx1 <- ddply(workday, .(interval), summarize, freq=length(steps), tot=sum(steps)/freq)
wx2 <- ddply(offday, .(interval), summarize, freq=length(steps), tot=sum(steps)/freq)
wx1 <- wx1[,-2]
wx2 <- wx2[,-2]
par(mar = c(1,1,0.75,1), oma = c(2,5,2,5), mfrow = c(2,1), cex.main = 0.75)
with (wx1, plot(tot~interval, type = 'l', xaxt = 'n', main = 'Weekday'))
with(wx2, plot(tot ~ interval, type = 'l', main = 'Weekend'))
mtext("Weekday vs Weekend", side=3, line=1, cex=1, col="blue", outer=TRUE)
