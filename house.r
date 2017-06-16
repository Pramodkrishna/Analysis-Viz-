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
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5))))
corr.idx

corrplot(as.matrix(correlations[corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
"Plotting to see which variables have the higher correlation, seems most of them have it "

correlations_price <- as.data.frame(correlations)
head(sort(correlations_price$SalePrice,decreasing = T))


"Develop the model "
train_price <- lm(formula = SalePrice ~ OverallQual+YearBuilt, data = train_num,na.action='na.exclude')
train_price

"Splitting the test data into numeric data "
test_price <- select_if(test.csv,is.numeric)
test_price[is.na(test_price)] <- 0
test_price$price <- as.numeric('null')

"Applying the predict function"

Test_price  <- predict(train_price,test_price,inteval = 'confidence')

"Comparing the means, we can also choose Anova method but this should also be fine "
mean(Test_price)
#mean(train_num$SalePrice)

"Seeing that the means are almost the same, we can say that our predicted model based
on considering only the numeric variables seems to be good "
"We should look into multicollinearity in the next step"


"Better way to find correlation and then use it for prediction compared to the above method" 

s1_data <- train_num

View(s1_data)
sapply(s1_data, function(x) sum(is.na(x)))
"Finding which var have highest Na"
"We see that LotFront + garageyearbuilt and MasVnr have the highest missing values"
    "Repalce them with the mean values"
s1_data$LotFrontage[is.na(s1_data$LotFrontage)] <- 80
s1_data$GarageYrBlt[is.na(s1_data$GarageYrBlt)] <- 1980
s1_data$MasVnrArea[is.na(s1_data$MasVnrArea)] <- 103

"Always replace the Na values before we perform the Corr"

"Finding the correlation"
s1_cor <- as.data.frame(cor(s1_data))
s1_cor_fin <- as.data.frame(s1_cor)

"Taking only the Correlated vairbale of Price with other var"
s1_cor_price <- as.data.frame(s1_cor[38])


summary(s1_cor_price$SalePrice)
s1_cor_price$SalePrice[is.na(s1_cor_price$SalePrice)] <- 0
s1_cor_price$names <- colnames(s1_cor)
View(s1_cor_price)

"Order the data"
s1_cor_price <- s1_cor_price[order(s1_cor_price$SalePrice,decreasing = T),]


"Selecting var which have cor value more than 0.5"
s1_price <- as.data.frame(s1_cor_price[which(s1_cor_price[,1]>0.5),])

s1_price$names


s1_lm1 <- lm(data = s1_data,formula = SalePrice ~ OverallQual+GrLivArea+GarageCars
                                              +GarageArea+TotalBsmtSF+X1stFlrSF
                                              +FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd)



price_pred <- predict(s1_lm1,test_price,interval = 'confidence')

head(price_pred)


"Selecting var which have cor more than 0.3"
s1_price2 <- as.data.frame(s1_cor_price[which(s1_cor_price[,1]>0.3),])

View(s1_price2)
s1_price2$names


s1_lm2 <- lm(data = s1_data,formula = SalePrice ~ OverallQual+GrLivArea+GarageCars
             +GarageArea+TotalBsmtSF+X1stFlrSF
             +FullBath+TotRmsAbvGrd+YearBuilt+YearRemodAdd+MasVnrArea
             +Fireplaces+GarageYrBlt+BsmtFinSF1+LotFrontage+WoodDeckSF+X2ndFlrSF+
               OpenPorchSF)


price_pred2 <- predict(s1_lm2,test_price)

"Conlusion: The mean of the first model is much closer to the mean of the house prices when compared to the second model" 

  


