# the evil chapter 5
#finall back to the code itself

#note: Because the professor explain the content so poorly
#I need to restudy this chapter again.
#its fucking hell
#2023/0204/2026 


#so this chapter is about the time series regressin models
  #feed the data and generate the a linear regression model
  #to future tie: its the shit you learned from 399
  #its scary because just the name is scar .

# the linear model.
  # the simple linear regression, you can see it from the BLUE, best linear regression omodel

library(fpp2)
library(GGally)
#lode the package
########################


autoplot(uschange[,c("Consumption","Income")]) +
#the data set uschange and pick the vartiable consumption and income
  ylab("% change") + xlab("Year")
#the ylab the percentage of change and x labor year 
#generate the graphy
###########

uschange %>%
  as.data.frame() %>% 
  # transfer the data uschange into the the data from
  ggplot(aes(x=Income, y=Consumption)) +
  #make the grphay of x as variable income and y as the variable consumption.
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
  #it using the geom_smooth function to genertte and does not need to show se
  #"geom_smooth" 函数用于在绘图中增加一条估计数据趋势的平滑曲线


<<<<<<< HEAD
#plot function in R to create a scatter plot with a smoothed line. One way to 
#achieve this is by using the l and then plotting the data and the smoother using the lines and points 
#functions. Here's an example:

x <- seq(1- 10)
y <- x + rnorm(100)
fit <- loess(y ~ x) 
# The function of the loess can be using here
oess function to fit a lowess smoother to the 
#data,
plot(x, y, pch = 19)
lines(predict(fit), col = "red")

##########################




=======
>>>>>>> main
#> `geom_smooth()` using formula 'y ~ x'

tslm(Consumption ~ Income, data=uschange) #强制拟合
#> 
#> Call:
#> tslm(formula = Consumption ~ Income, data = uschange)
#> 
#> Coefficients:
#> (Intercept)       Income  
#>       0.545        0.281
#>       

# the multiple linear regression 

#############################

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

#write again as normal paper does

GGally::ggpairs(as.data.frame (uschange))


###############################

#the assumption
  #they have mean zero; otherwise the forecasts will be systematically biased.
    #they are not autocorrelated; otherwise the forecasts will be inefficient, 
#as there is more information in the data that can be exploited.
  #they are unrelated to the predictor variables; otherwise there would
    #be more information that should be included in the systematic part of the model.

########################################################################################################################################################################################################
#5.2 the least squares estimation
##############################################################################################################################################################################

#2023/02/05
#note here: using the lm() function to constructed the models that you will using in the data.
#then, using the summary function to find out which one one is the best. 
#AIC/BIC are good for the small sample size
#the ridge and lasso regresion can tell you which one is better in the relative big sample size.
  #they can also tell you how many variance is the best for the selected the variance. 


#what is the least squares estimation?
  #It give the least value for hte sum of squared errors. Finding the best estimates of the coefficients s
  #often called "fitting" the model to the data. or somtimes "learning or traning the model. 

#The tslm() function fits a linear regression model to time series data. It is similar to the lm() 
#function which is widely used for linear models, but tslm() provides additional facilities for handling time series.


fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings, 
  #the variance off consumption, income, production, unemplotmnet and saving
  data=uschange)
summary(fit.consMR)

autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))


cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

# the goodness of git
  # a common way to summariee how well a linear regression model fits the data is via the coefiicent of determination
  #R^2 value will nevver degree when adding an extra predictor to the model and this can leand to over fitting. 

# The standard error of regression. 


# 5.3 evaluating the regression model 
  #the AFC grahic
  #you can usin the function of the checkresiduals() to do it.
  #the top one is check the 


  #residual plots aainest predictos

df <- as.data.frame(uschange)
#transfer the data uschange into the data frame 
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
#then using the residuals function to get the data
#using the residuals function to calculate it and add
#into the df which already into the df(uschange)

p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
#drew the graphic of the income and residuals

p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
#drew the grapic of the production and residuals

p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
#same

p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
#same

#generate graphy
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

#2023/02/05 
  #note the following the function does not work...

#if a pattern is ovserved, there may be heroscedasticity, in the error  which means that
#the variance of the residuals may not be constant.

#what the fuck is the heroscedasticity? 
  #the non linear relationship between the data....
  #the like the error is not the same and avager to the zero
  #and so one
  #https://www.zhihu.com/question/354637231 
  #check it chinese


#outliers and influence ovbservations
  #异常值和有影响力的观察 or in other workd 
  #the outliers - we need to find it and and delete it in order to get out of the 
  #the influence of the shit

  #If the high r^2 and the high residuals autocorrelation can be the sign of spurious regression,

########################################################################
#5.4 some useful predictors
  #Trend
    #you can using the function tslm() and using the trend....

  #dummy variable
    #we can using it to cover or end the effecf of the outlier
    #forexample, when forecasting tourist arrvals to barizil, we will needt to account for th
    #the effect of the rio olympic in 2016

#seasonal dummy variable 
  #Suppose that we are forecasting daily data and we want to account for the day of the week as a predictor.
  #Then the following dummy variables can be created.

  #####
    #So for quarterly data, use three dummy variables; for monthly data, 
    #use 11 dummy variables; and for daily data, use six dummy variables, and so on.
  ####

beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")


fit.beer <- tslm(beer2 ~ trend + season)
#set the model tsslm
summary(fit.beer)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-42.903  -7.599  -0.459   7.991  21.789 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 441.80044    3.73353 118.333  < 2e-16 ***
 # trend        -0.34027    0.06657  -5.111 2.73e-06 ***
#  season2     -34.65973    3.96832  -8.734 9.10e-13 ***
#  season3     -17.82164    4.02249  -4.430 3.45e-05 ***
#  season4      72.79641    4.02305  18.095  < 2e-16 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 12.23 on 69 degrees of freedom
#Multiple R-squared:  0.9243,	Adjusted R-squared:  0.9199 
#F-statistic: 210.7 on 4 and 69 DF,  p-value: < 2.2e-16


autoplot(beer2, series="Data") +
  autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")



cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)


#这是一个R语言中的代码，它的意思是将两个数据集（"Data"和"Fitted"）绑定在一起，
#然后转换为数据帧，并使用ggplot库绘制出一个散点图，其中x轴为实际值，y轴为拟合值
#，每个散点的颜色代表不同的季度，图标题为"季度啤酒生产量"，颜色使用"Dark2"调色板
#，并且在图上画一条斜率为1，截距为0的直线。


#Trading days
  #The number of trading days in a month can vary considerably and can have a substantial effect on sales data.
  #To allow for this, the number of trading days in each month can be included as a predictor.

#Easter
  #It is often useful to include advertising expenditure as a predictor. However, 
  #since the effect of advertising can last beyond the actual campaign, we need to include lagged values of advertising expenditure. 

#Easter
  #you can use this function to get the date of easter
  #ntoe: why???? based 2023/02.23 

#Fourier series
  #An alternative to using seasonal dummy variables, especially for long seasonal periods, is to use Fourier terms.
  #so, where to use it?
    #when the data are in the trend that are not seasonal?

#############################

#5.5

# adjust R^2
  #Maximising ¯R2 works quite well as a method of selecting predictors,
  #although it does tend to err on the side of selecting too many predictors.


#Cross-validation
  #the best fit is the one with the smallest cv

#AIC and BIC 
  #the model with. the minimum value of the AIC is of then best model for forecasting.
  #as with the AIC, the AICc should be minimised

  #BIC test 
    #As with the AIC, minimising the BIC is intended to give the best model. 
    #The model chosen by the BIC is either the same as that chosen by the AIC
    #or one with fewer terms. this is because the BIC penalises the number of 
    #parameters more heabily than the AIC. for the large values of T, minimising
    #BIC is similar to leave v out cross validation when 

#which measure should we use?
  # Not R^2 because it tendenct to select too many perdictor variables makes it less sutiable
  # for forcasting.

  #not BIC
    #the BIC will select that model given enough data. 
    #hoever, in reality, there is rarely, if even a 
    # true underlying modle....
    # selecting that mode lwill not nevcessarily given the best forecast because
    # the parameter estimates may not be accuratte.

#Consequently, we recommend that one of the AICc, AIC, 
#or CV statistics be used, each of which has forecasting as their objective. 
#If the value of T is large enough, they will all lead to the same model. 
#In most of the examples in this book, we use the AICc value to select the forecasting 
#model.


#Best subset regreiion
  #start with the model containing all potential predictors.
  #Remove one predictor at a time. Keep the model if it improves the measure of predictive accuracy.
  #Iterate until no further improvement.

#Beware of inference after selecting predictors
  #if you do with to look at the statistical significance of the predictors, beware
  #that ay procedure involving selecting predictors first will invalidate the
  #assuptions behind the p values 

  #the procedictors first will invalidate the assumption behind the p values. 
  #the procedures wr recommoend for selecting predictors are helpfup when the model is used
  #for forecasting, they are no helpful if you iteh t



###################
#5.6
  #forcasting with regression 
  























