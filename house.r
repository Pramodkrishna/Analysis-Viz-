setwd("/home/pramod/Documents/Kaggle/House price/")
#install.packages("corrplot")
#install.packages("car")
#install.packages("Metrics")

library(corrplot)
library(Metrics)
require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plo

test.csv <- read.csv("test.csv",stringsAsFactors = F)
train.csv <- read.csv("train.csv",stringsAsFactors = F)

test.csv$Saleprice <- "Null"


"Splitting the data into Character and Numeric data frames "
train_num <- select_if(train.csv,is.numeric)
train_char <- select_if(train.csv,is.character)

"Adding Sale price and ID to Character DF"

train_char$Price <- train.csv$SalePrice
train_char$ID <- train.csv$Id
train_char <- train_char[,c(ncol(train_char),1:ncol(train_char)-1)]
colnames(train_char)

"Drawing table to test charac vs numeric data "
table(train_char$MSZoning)
table(train_char$Alley)
table(train_char$LandContour)
table(train_char$Fence)
table(train_char$PoolQC)
table(train_char$GarageType)

"Perform glm"
salecond_saleprice <- glm(train_char$Price~train_char$SaleCondition,data = train_char)
Saleprice_poolqc <- glm(train_char$Price~ train_char$PoolQC,data = train_char)
summary(Saleprice_poolqc)
#linebreaker

##Kag: "https://www.kaggle.com/tannercarbonati/detailed-data-analysis-ensemble-modeling/code"##
#Correlation of Numeric Data with Sale price 
"Correlation"
correlations <- cor(train_num)

corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
 "Sorting the data "
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
"Plotting to see which variables have the higher correlation, seems most of them have it "

correlations_price <- as.data.frame(correlations)
head(sort(correlations_price$SalePrice,decreasing = T))


correlations_price <- as.data.frame(correlations)
"Develop the model "
train_price <- lm(formula = SalePrice~. ,data = train_num,na.action='na.exclude')
train_price

"Splitting the test data into numeric data "
test_price <- select_if(test.csv,is.numeric)
test_price[is.na(test_price)] <- 0
test_price$price <- as.numeric('null')

"Applying the predict function"

Test_price  <- predict(train_price,test_price,inteval = 'confidence')

"Comparing the means, we can also choose Anova method but this should also be fine "
mean(Test_price)
mean(train_num$SalePrice)

"Seeing that the means are almost the same, we can say that our predicted model based
on considering only the numeric variables seems to be good "
"We should look into multicollinearity in the next step"