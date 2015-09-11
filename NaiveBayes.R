library(readxl)
library(tm)
library(dplyr)
library(caret)
library(e1071)

train_data <- read_excel("challenge_training data.xlsx")
test_data <- read_excel("challenge_submission.xlsx")
train_id <- createDataPartition(train_data$Author_Category, times=10,p=0.7, list=F)


    training <- train_data[train_id,]
    desc <- training$Description
    dtm <- terms(desc)
    dtms <- removeSparseTerms(dtm,0.99)
    m <- as.matrix(dtms)
    m<- as.data.frame(m)
    author_cat <- training$Author_Category
    m <- cbind(m,author_cat)
    m2<-m[complete.cases(m),]
    
    validation <- train_data[-train_id,]
    desc_val <- validation$Description
    author <- validation$Author_Category
    dtm_v <- terms(desc_val)
    dtms_v <- removeSparseTerms(dtm_v,0.99)
    m1 <- as.matrix(dtms_v)
    m1 <- as.data.frame(m1)
    m1<- cbind(m1,author)
    model <- naiveBayes(author_cat ~ ., data = m)
    pred <- predict(model,m1)
    
    pred1<- knn(train=m,test=m1,cl=author_cat)
