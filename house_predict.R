# "Prediction of House Prices (Regression Trees, Random Forests, Gradient Boosting Machine)"
library(dplyr)
library(tidyr)
library(rpart)
library(randomForest)
library(ggplot2)
library(gbm)
library(Metrics)

# Data preparation


# Load data

train_pred <- read.csv("train.csv",stringsAsFactors = F)

row.names(train_pred) <- train_pred$Id
train_pred <- train_pred[,-1]
train_pred[is.na(train_pred)] <- 0
for(i in colnames(train_pred[,sapply(train_pred,is.character)])){
  train_pred[,i] <- as.factor(train_pred[,i])
}


model_kag <- rpart(SalePrice ~., data = train_pred, method = "anova")
predict_kag_pred <- predict(model_kag, test.csv)
head(predict_kag_pred)
head(train.csv$SalePrice)

# RMSE
Root_mean <- rmse(actual =  train.csv$SalePrice,predicted = predict_kag_pred)
View(Root_mean)
plot1 <- predict_kag_pred~train_num$SalePrice
plot2 <- train_num$SalePrice~predict_kag_pred
