library(readxl)
library(tm)
library(dplyr)
library(caret)

train_data <- read_excel("challenge_training data.xlsx")
train_id <- createDataPartition(train_data$Author_Category, times=10,p=0.7, list=F)
train_terms <- list()
for (i in 1:10)
{
  training <- train_data[train_id[,i],]
  desc_HCP <- training %>% filter(Author_Category=="HCP") %>% select(Description)
  
  txt.desc <- VectorSource(desc_HCP)
  txt.desc <- Corpus(txt.desc)
  docs <- tm_map(txt.desc, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("tweets","views", "director", "university", "opinions"))
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removeNumbers)
  dtm <- DocumentTermMatrix(docs)
  freq_terms<- findFreqTerms(dtm, lowfreq = 40)
  train_terms[[i]]<- freq_terms
}

for(y in 1:10)
{
validation <- train_data[-train_id[,10],]
for (z in 1: dim(validation)[1])
{
desc_val <- validation$Description
txt.desc <- VectorSource(desc_val[z])
txt.desc <- Corpus(txt.desc)
docs <- tm_map(txt.desc, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("tweets","views", "director", "university", "opinions"))
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, removeNumbers)
dtm <- DocumentTermMatrix(docs)
test_terms <- findFreqTerms(dtm) 
x<-match(test_terms,train_terms[[10]], nomatch=0)
if (any(x!=0))
{
  validation$Author_Category_Pred[z] <- "HCP"
}else 
{
  validation$Author_Category_Pred[z] <- "Non_HCP"
}
}
}
