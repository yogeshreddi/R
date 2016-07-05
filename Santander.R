library(randomForest)
library(gbm)
library(dplyr)


###importing data

train <- read.csv("E:/Study/Data Challenges/kaggle/Santander/train/train.csv")
test <- read.csv("E:/Study/Data Challenges/kaggle/Santander/train/test.csv")

####Feature speration based on class type


### Missing Values
sum(sapply(train, function(x) sum(is.na(x))))


#### To remove id columns and constant columns

features_to_remove <- function(df){
  features_train = c()
  for(i in 1:ncol(df)){
    if(length(unique(df[,i,]))==nrow(df)){
      print(names(df[i]))
      features_train<- c(features_train,names(df[i]))
    }
    if (var(train[,i,])==0)
    {
      features_train = c(features_train,names(train[i]))
    }
    i=i+1
  }
  return(features_train)
}


test_id <- test$ID

train_target <- train$TARGET

train <- select(train,-TARGET)

#### Removing unwanted features

features_to_remove = features_to_remove(train)

features_to_use = setdiff(names(train),features_to_remove)

train <- train[features_to_use]
test <- test[features_to_use]

##########Function to get the correlations between numerical varibales
cor_range<-function(x,limit)
{
  col1<-c()
  col2<-c()
  corr<-c()
  for (i in 1:ncol(x))
  {
    for (j in 1:ncol(x))
    {
      if(!is.numeric(x[,j]))
      {
        warning("Dataset contains Factor variables, Correlation can only be evaluated for numeric variables") 
        return(names(x[j]))
      }
      else
      {
        if(abs(cor(x[,i],x[,j]))>=limit & names(x[i])!=names(x[j])){
          col1<-c(col1,names(x[i]))
          col2<-c(col2,names(x[j]))
          corr<-c(corr,cor(x[,i],x[,j]))
        }    
      }
    }
  }  
  data.frame(cbind(col1,col2,corr))
}



##### Fitting Random forest 

fit_rf <- randomForest(x = train,y = as.factor(train_target),ntree = 1000)
