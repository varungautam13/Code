library(dplyr)
train <- read.csv("Train_GC37HGt.csv")
blank_race <- train[train$Race=="",]

table(train$State_Country ,train$Var3)
blank_Var2 <- train %>% filter(is.na(Var2))
blank_Var3 <- train %>% filter(is.na(Var3))
grouped_data_2 <- train %>% group_by(State_Country,Year, Gender,AgeGroup,Ethnicity,Race) %>%
    summarise(var2 = mean(Var2,na.rm=T))
grouped_data_3 <- train %>% group_by(State_Country,Year,Gender,AgeGroup,Ethnicity,Race) %>%
    summarise(var3 = mean(Var3,na.rm=T))


