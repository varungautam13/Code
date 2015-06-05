library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(gridExtra)

## Reading CAT data for all Session Keys
CAT_Data <- read_excel("CATData_3.xlsx", sheet = 1)
Session_Data <- read_excel("CATData_3.xlsx", sheet = 3)

## splitting up CAT data for each Session Key
CAT_Data_list <- split(CAT_Data, CAT_Data$Session_Key)
Session_Data_list <- split(Session_Data, Session_Data$Session_Key)

## Extracting names of Category (Client Needs) for which plots will be created 
Category_vector <- unique(CAT_Data$Category_Name)
Category_Names <- Category_vector[c(1,3,4,5,6,8,9,11)]

## Removing variable to free up system memory
rm(CAT_Data,Category_vector, Session_Data)

## Looping for each student (Session Key) and creating plots for each student
for (m in 1:length(CAT_Data_list)){
    
    ## Creating Filter_Category for each student
    Filter_Category <- CAT_Data_list[[m]] %>% 
        filter(Question_Correct == 1) %>% 
        select(Sequence_Number, Category_Name, Percentage, Actual_Value) %>%
        filter(Sequence_Number %in% seq(5,155,5)) %>%
        mutate(Actual_Percentage = (as.integer(Actual_Value)/as.integer(Sequence_Number))*100) %>% 
        select(-4) 
        
    names(Filter_Category)[3] <- "Blueprint"
    CAT_Score<-as.numeric((summarise(Session_Data_list[[m]],CAT_Score=last(as.numeric(Theta)))+3)*3.75 )
    plot_list<- list()
    
    for (n in 1:length(Category_Names) ) {
        Filter_Category1 <- Filter_Category %>% filter(Category_Name == Category_Names[n]) %>%
            gather(Category,Percentages,Blueprint,Actual_Percentage) %>%
            select(Sequence_Number,Category,Percentages) %>%
            mutate(Category = ifelse(Category == "Actual_Percentage", Category_Names[n], "Blueprint"))
        plot<-ggplot(Filter_Category1, aes(x=as.numeric(Sequence_Number), y=round(as.numeric(Percentages),2),  colour=Category)) + geom_line() +  xlab(Category_Names[n]) + ylab("") + theme_grey()  + theme(legend.position = "none")  + theme(axis.title.x=element_text(size=9, lineheight=.9, face="bold"))
        plot_list[[n]]<-plot
    }
    
    pdf(paste("CAT_TestTaker_", m, ".pdf"))
        grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]], ncol=2, nrow=4, widths = unit(0.5,"npc"), heights = unit(0.25,"npc"), main = paste("\n Category Blueprint Vs Actual Percentages [Score:", CAT_Score,"]"))
       dev.off()
}
rm(list=ls(all=TRUE))

