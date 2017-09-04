
setwd("Documents/Kaggle/House/")
library(e1071)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(rpart)
library(randomForest)
library(ggplot2)

train <- read.csv("train.csv",header = T)
train[is.na(train)] <- 0

test <- read.csv("test.csv",header = T)
y_train=train$SalePrice

"""
Putting the value of sale price in a different variable 
Removing the prediction or value to be predicted from the Train data set which is the sale price 
Removing the Id from both the data sets 
"""

"""
Data Cleaning

combine the data,cause we have missing values in both we do it for efficient cleaning 
Generally we can skip the step if we dont need it

"""

train_num <- select_if(train,is.numeric)
test_num <- select_if(test,is.numeric)

train_num[is.na(train_num)] <- 0
test_num[is.na(test_num)]<- 0 
test_num$SalePrice <- 0
price_svm <- svm(y_train~.,data = train_num)

train$SalePrice
head(test_num$SalePrice)

price_predict <- predict(price_svm,train_num)
price_predict
table(price_predict,y_train)


price_test_pred <- predict(price_svm,test_num)

test_num$SalePrice <- price_test_pred


qplot(test_num$SalePrice,stat_bin = 45)
qplot(train_num$SalePrice)
###################################################

#No point in using anova
price_anv  <- rpart(SalePrice ~., data = train_num, method = "anova")
predict_price_anova <- predict(price_anv,test_num)


###################################################


price_lm <- lm(train_num$SalePrice~.,data = train_num)
predict_price_lm <- predict(price_lm,test_num)
#########################################
qplot(predict_price_lm)
qplot(predict_price_anova)
qplot(price_test_pred)

