library(tidyr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,destfile = "repdata_data_StormData.csv.bz2")
storm_data <- read.csv("repdata_data_StormData.csv.bz2")

storm_impact <- storm_data %>% 
        group_by(EVTYPE) %>% 
        summarise(fatalities = sum(FATALITIES), injuries = sum(INJURIES), total = sum(fatalities, injuries)) 

evtype <- storm_impact %>% 
        arrange(desc(total)) %>% 
        summarise(first(EVTYPE))
storm_tidy <- storm_impact %>% 
        gather(type,count,2:4)
storm_filter <- storm_tidy %>% arrange(desc(count)) %>%
        group_by(type) %>%
        # arrage(desc(count))
         slice(1:5)
ggplot(storm_filter,aes(EVTYPE,count,fill=type)) + geom_bar(stat="identity",position="dodge")
