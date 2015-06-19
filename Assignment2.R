library(tidyr)
library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,destfile = "repdata_data_StormData.csv.bz2")
storm_data <- read.csv("repdata_data_StormData.csv.bz2")

health_impact <- storm_data %>% 
    group_by(EVTYPE) %>% 
    summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES), total = sum(fatalities, injuries)) 

economic_impact <- storm_data %>% 
    group_by(EVTYPE) %>% 
    summarise(crop_dmg = sum(CROPDMG), prop_dmg = sum(PROPDMG), total_dmg = sum(crop_dmg, prop_dmg)) 

health_impact_tidy <- health_impact %>% 
    gather(type,count,2:4) %>% 
    group_by(type) %>%
    arrange(desc(count)) %>%
    slice(1:10)

economic_impact_tidy <- economic_impact %>% 
    gather(type,count,2:4) %>% 
    group_by(type) %>%
    arrange(desc(count)) %>%
    slice(1:10)

split_health <- split(health_impact_tidy, health_impact_tidy$type)
ggplot(split_health[[1]],aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity",colour="black") + ggtitle("No. of Fatalities for top 10 Events") + xlab("Event Type") + ylab("No. of Fatalities") +guides(fill=FALSE)
ggplot(split_health[[2]],aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity", colour="black") + ggtitle("No. of Injuries for top 10 Events") + xlab("Event Type") + ylab("No. of Injuries")
ggplot(split_health[[3]],aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity", colour="black") + ggtitle("Total no. of health impacts for top 10 Events") + xlab("Event Type") + ylab("Total no. of Health Impact")

split_economic <- split(economic_impact_tidy, economic_impact_tidy$type)
ggplot(split_economic[[1]],aes(EVTYPE,count/1000,fill=type)) + geom_bar(stat="identity",colour="black") + ggtitle("Crop Damage Amount for top 10 Events") + xlab("Event Type") + ylab("Crop Damage Amount") +guides(fill=FALSE)
ggplot(split_economic[[2]],aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity",colour="black") + ggtitle("Property Damage Amount for top 10 Events") + xlab("Event Type") + ylab("Property Damage Amount") +guides(fill=FALSE)
ggplot(split_economic[[3]],aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity",colour="black") + ggtitle("Total Damage Amount for top 10 Events") + xlab("Event Type") + ylab("Total Damage Amount") +guides(fill=FALSE)
