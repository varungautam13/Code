library(data.table)
library(ggplot2)
library(lubridate)

### Part 1
file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file_url,destfile="activity.zip")
unzip("activity.zip")
activity_data <- fread("activity.csv")

### Part 2
total_steps <- activity_data[,list(tot_steps = sum(steps,na.rm=T)),by=date]

ggplot(total_steps,aes(x=tot_steps) + geom_histogram()  

mean_steps <- total_steps[,mean(tot_steps)]   
median_steps <- total_steps[,median(tot_steps)]   

### Part 3
avg_interval <- activity_data[,list(avg_steps = mean(steps,na.rm=T)),by=interval]
ggplot(avg_interval, aes(interval,avg_steps)) + geom_line() 

max_interval <- avg_interval[order(avg_steps,decreasing=T),first(interval)] 

### Part 4
x <- dim(activity_data[!complete.cases(activity_data)])[1]   
x 
setkey(activity_data,interval)
setkey(avg_interval,interval)

new_activity_data <- activity_data[avg_interval,steps:= ifelse(is.na(steps),avg_steps,steps)]

total_steps_new <- new_activity_data[,list(tot_steps = sum(steps,na.rm=T)),by=date]
mean_steps_new <- total_steps_new[,mean(tot_steps)]   
median_steps_new <- total_steps_new[,median(tot_steps)]           

ggplot(total_steps_new,aes(x=tot_steps)) + geom_histogram()  

### part 5
total_steps_new <- total_steps_new[,day := ifelse(wday(date) %in% c(2:6),"weekday","weekend")]
total_steps_new[,"date"] <- ymd(total_steps_new[,"date"])
b <- b %>% mutate(day=ifelse(wday(date) %in% c(2:6),"weekday","weekend"))

ggplot(b,aes(interval,steps)) + geom_line() + facet_wrap(~day, nrow=2)
