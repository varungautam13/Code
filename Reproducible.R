activity <- read.csv("activity.csv")
##activity$date <- as.Date(activity$date,format="%y%m%d")
steps <- activity %>% 
    group_by(date) %>% 
    summarise(steps = sum(steps,na.rm=T))
avg_steps <- activity %>% 
    group_by(date) %>% 
    summarise(average = mean(steps,na.rm=T))
median_steps <- activity %>% 
    group_by(date) %>% 
    summarise(med = median(steps,na.rm=T))
a <- activity %>% 
    group_by(interval) %>% 
    summarise(avg_step = mean(steps,na.rm=T)) 
max_avg <- as.numeric(a %>%  
                          arrange(desc(avg_step)) %>%
                          summarise(max_avg = first(interval)))
hist(steps$steps)
plot(a$interval,a$avg_step, type="l")
x <- which(!complete.cases(activity))

b <- inner_join(activity,a,by="interval") 
b<- b %>% mutate(steps=ifelse(is.na(steps),avg_step,steps)) %>% select(-4)

steps_b <- b %>% 
    group_by(date) %>% 
    summarise(steps = sum(steps,na.rm=T))
avg_steps_b <- b %>% 
    group_by(date) %>% 
    summarise(average = mean(steps,na.rm=T))
median_steps_b <- b %>% 
    group_by(date) %>% 
    summarise(med = median(steps,na.rm=T))

hist(steps_b$steps)

b$date <- ymd(b$date)
b <- b %>% mutate(day=ifelse(wday(date) %in% c(2:6),"weekday","weekend"))

g<-ggplot(b,aes(interval,steps))
g + geom_line() + facet_wrap(~day, nrow=2)
