#Reference https://www.r-bloggers.com/7-visualizations-you-should-learn-in-r/ 
#Try using the viz from kernels in Kaggle 
library(ggplot2)
#Visualization for cor.test btw year built and sale price 
#Scatter plot
"Scatter Plot is used to see the relationship between two continuous variables."

ggplot(train_num, aes(train_num$YearBuilt, train_num$SalePrice)) + geom_point(aes(color = train_num$YearBuilt)) + scale_x_continuous("Year built", breaks = seq(1872,2010,15)) + theme_bw() + facet_wrap( ~ train_num$YearBuilt)

#Histogram 
"Histogram is used to plot continuous variable. 
It breaks the data into bins and shows frequency distribution of these bins. 
We can always change the bin size and see the effect it has on visualization".

ggplot(train_num,aes(train_num$YearBuilt)) + geom_histogram(binwidth = 2) +
  scale_x_continuous("year",breaks = seq(1872,2010,15)) +
  scale_y_continuous("Price",breaks = seq(100000,755000,35000))


#Bar & Stack Bar Chart
"Bar charts are recommended when you want to plot a categorical variable 
or a combination of continuous and categorical variable."
#Using the train_chara data 
names(train_char)

ggplot(train_char,aes(train_char$Condition1)) + geom_bar(fill = "Black") + theme_light()
  scale_x_continuous("Establishment Year") + scale_y_continuous("Count", breaks = seq(1,1460,1)) 
  
ggplot(train_char,aes(train_char$BldgType)) + geom_bar(fill = "Black") + theme_light()
scale_x_continuous("Establishment Year") 


#Stacked Bar chart: Advanced bar charts, we generally use 2 categorical variables here
ggplot(train_char , aes(train_char$SaleCondition, fill = train_char$Neighborhood)) + geom_bar()
 
ggplot(train_char , aes(train_char$SaleCondition, fill = train_char$Condition1)) + geom_bar()



#Box Plot
"Box Plots are used to plot a combination of categorical and continuous variables. 
This plot is useful for visualizing the spread of the data and detect outliers."

ggplot(train_char, aes(train_char$Price, train_char$Condition1)) + geom_boxplot(fill = "Black")

ggplot(train_char, aes(train_char$SaleCondition, train_char$HouseStyle)) + geom_boxplot(fill = "Black")




