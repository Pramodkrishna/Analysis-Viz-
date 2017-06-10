setwd("/home/pramod/Documents/Kaggle/House price/")
#install.packages("corrplot")
#install.packages("car")
library(dplyr)
library(ggplot2)
library(xgboost)
library(corrplot)
library(car)
test.csv <- read.csv("test.csv",stringsAsFactors = F)
train.csv <- read.csv("train.csv",stringsAsFactors = F)
test.csv$Saleprice <- "Null"


train_num <- select_if(train.csv,is.numeric)
train_char <- select_if(train.csv,is.character)

train_char$Price <- train.csv$SalePrice
train_char$ID <- train.csv$Id
train_char <- train_char[,c(ncol(train_char),1:ncol(train_char)-1)]
View(train_char)



unique(train_char$Condition1)
unique(train_char$Condition2)
str(train_char)

char_unq <- sapply(train_char[,-45],unique)


salecond_saleprice <- glm(train_char$Price~train_char$SaleCondition,data = train_char)
Saleprice_poolqc <- glm(train_char$Price~ train_char$PoolQC,data = train_char)
summary(Saleprice_poolqc)





table(train_char$Condition1)

