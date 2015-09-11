## Model
library(readxl)
library(tm)
library(dplyr)
library(caret)

train_data <- read_excel("challenge_training data.xlsx")
test_data <- read_excel("challenge_submission.xlsx")
train_id <- createDataPartition(train_data$Author_Category, times=10,p=0.7, list=F)
train_terms_HCP <- list()
train_terms_NHCP <- list()
for (i in 1:10)
{
    training <- train_data[train_id[,i],]
    desc_HCP <- training %>% 
                         filter(Author_Category=="HCP") %>% 
                         select(Description)
    desc_NHCP <- training %>% 
                            filter(Author_Category=="NON_HCP") %>% 
                            select(Description)
    freq_terms_HCP <- terms(desc_HCP)
    train_terms_HCP[[i]]<- freq_terms_HCP
    freq_terms_NHCP <- terms(desc_NHCP)
    train_terms_NHCP[[i]] <- freq_terms_NHCP
    
}
train_terms_H <- unique(unlist(train_terms_HCP))
train_terms_N <- unique(unlist(train_terms_NHCP))
x <- match(train_terms_N,train_terms_H, nomatch=0) 
x<- x[x!=0]
train_terms_H <- train_terms_H[-6]

    validation <- train_data[-train_id[,a],]
    for (z in 1: dim(validation)[1])
    {
        desc_val <- validation$Description
        test_terms <- terms(desc_val[z], test=1)
        
        x<-match(test_terms,train_terms_H, nomatch=0)
        if (any(x!=0))
        {
            validation$Author_Category_Pred[z] <- "HCP"
        }else 
        {
            validation$Author_Category_Pred[z] <- "Non_HCP"
        }
    }

table(validation$Author_Category, validation$Author_Category_Pred)

## text mining Function
terms <- function(desc, test=0){
txt.desc <- VectorSource(desc)
txt.desc <- Corpus(txt.desc, readerControl = list(language = "en"))
docs <- tm_map(txt.desc, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("tweets","views", "director", "university", "opinions"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeNumbers)
toSpace <- content_transformer(function(x , pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "-")
# docs <- tm_map(docs, toSpace, "(")
# docs <- tm_map(docs, toSpace, ")")
dtm <- DocumentTermMatrix(docs)
if(test==0){
freq_terms<- findFreqTerms(dtm, lowfreq = 40)}
else{
    freq_terms<- findFreqTerms(dtm)  
}
}

## Prediction
for (z in 1: dim(test_data)[1])
{
    desc_val <- test_data$Description
    test_terms <- terms(desc_val[z], test=1)
    
    x<-match(test_terms,train_terms_H, nomatch=0)
    if (any(x!=0))
    {
        test_data$Author_Category[z] <- "HCP"
    }else 
    {
        test_data$Author_Category[z] <- "Non_HCP"
    }
}

test_data <- test_data %>% select(UID, Author_Category)
write.csv(test_data, file="Submission_challenge_2.csv")
