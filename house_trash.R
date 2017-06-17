setwd("Documents/Kaggle/House price/")
load(file = "house_predict.R")


setwd("/home/pramod/Downloads/")

kag_data <- read.csv("xgb.csv",header = T,stringsAsFactors = F)

correlations <- cor(train_num[,-38])
View(train_num)

corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
"Sorting the data "
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5))))
corr.idx

#corrplot(as.matrix(correlations[corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
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








#######################################################
s1_data <- train_num
colnames(s1_data)

sapply(s1_data, function(x) sum(is.na(x)))

summary(s1_data$LotFrontage)

s1_data$LotFrontage[is.na(s1_data$LotFrontage)] <- 80
mean(s1_data$LotFrontage)
max(s1_data$LotFrontage)

s1_data$GarageYrBlt[is.na(s1_data$GarageYrBlt)] <- 1980

sapply(s1_data,function(x) sum(is.na(x)))


s1_cor <- as.data.frame(cor(s1_data))
s1_cor_fin <- as.data.frame(s1_cor)
s1_cor_price <- as.data.frame(s1_cor[38])

summary(s1_cor_price$SalePrice)
s1_cor_price$SalePrice[is.na(s1_cor_price$SalePrice)] <- 0

s1_cor_price$SalePrice <- (sort(s1_cor_price$SalePrice,decreasing = T))
View(s1_cor_price)
s1_cor_price$names <- colnames(s1_cor)
View(s1_cor_price)


s1_price <- as.data.frame(s1_cor_price[which(s1_cor_price[,1]>0.5),])
#colnames(s1_price) <- c("Price")

