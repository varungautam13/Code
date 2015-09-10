## Part 1
library(readxl)
library(tm)
library(dplyr)
library(caret)
train_data <- read_excel("challenge_training data.xlsx")
 train_id <- createDataPartition(train_data$Author_Category, p=0.7, list=F)
training <- train_data[train_id,]
desc_HCP <- training %>% filter(Author_Category=="HCP") %>% select(Description)

validation <- train_data[-train_id,]
#test_data1 <- read_excel("challenge_submission.xlsx")
desc_test <- validation$Description
txt.desc <- VectorSource(desc_HCP)
txt.desc <- Corpus(txt.desc)
docs <- tm_map(txt.desc, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)
dtm <- DocumentTermMatrix(docs)
train_terms <- findFreqTerms(dtm, lowfreq = 50) 

## Part 2
for (i in 1: dim(validation)[1])
{
txt.desc <- VectorSource(desc_test[i])
txt.desc <- Corpus(txt.desc)
docs <- tm_map(txt.desc, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)
dtm <- DocumentTermMatrix(docs)
test_terms <- findFreqTerms(dtm) 
x<-match(test_terms,train_terms, nomatch=0)
if (any(x!=0))
{
    validation$Author_Category[i] <- "HCP"
}
else
{
    validation$Author_Category[i] <- "Non_HCP"
}
}
# test_data <- test_data %>% select(UID, Author_Category)
# write.csv(test_data, file="Submission_challenge_1.csv")
