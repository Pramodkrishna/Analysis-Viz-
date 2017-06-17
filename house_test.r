library(car)

#(Visualisation,correlations <- cor(train_num[,c(5,6,7,8, 16:25)], use="everything")
#corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
#summary(correlations)

#con_saletype <- cor(x = train_char$SaleCondition,
#                    y = train_char$SaleType)#


#price_yearsold

#pearson correlation
price_yearsold <- cor(x = train_num$SalePrice,
                        y = train_num$YrSold)

#price_yearsold  >>>> "-0.028"
#Indicates that we have less correlation between Year sold and Sale Price which makes sense

#spearman correlation
price_yearsold_spear <- cor(x = train_num$SalePrice,
                            y = train_num$YrSold,method = "spearman")

#kendall correlation
price_yearsold_kendal <- cor(x = train_num$SalePrice,
                            y = train_num$YrSold,method = "kendall")


#All of the test yielded the same correlation coefficient, indicating that we have less cor
#among these two var. 


#simple plot
plot(x = train_num$YrSold,
       y = train_num$SalePrice) +
        abline(lm(train_num$SalePrice~train_num$YrSold), col="red")


#Cor.test
price_yearsold_cortest <- cor.test(x = train_num$SalePrice,
         y = train_num$YrSold)

price_yearsold_cortest









#Year Built 
price_yearbuild <- cor(x = train_num$SalePrice,
                        y= train_num$YearBuilt)
summary(price_yearbuild)
price_yearbuild


scatterplot(x = train_num$YearBuilt,
            y= train_num$SalePrice,data = train_num) 
+ abline(lm(train_num$SalePrice~train_num$YearBuilt
  ,color = "Blue"))


price_yearbuild_test <- cor.test(x = train_num$SalePrice,
                                 y = train_num$YearBuilt )


price_yearbuild_test

#Conclusion
#Since the cor.test and cor function have given a positive value 
#We can conclude that year built more or less has an effect on the Sale price 


#
