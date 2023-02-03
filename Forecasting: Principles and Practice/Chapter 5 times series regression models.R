# the evil chapter 5
#finall back to the code itself

#note: Because the professor explain the content so poorly
#I need to restudy this chapter again.
#its fucking hell
#2023/0204/2026 


# the linear model.
  # the simple linear regression, you can see it from the BLUE, best linear regression omodel

install.packages("fpp2")
install.packages("GGally")
library(fpp2)
library(GGally)

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
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

install.packages("fpp2")

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

#the assumption
  #they have mean zero; otherwise the forecasts will be systematically biased.
    #they are not autocorrelated; otherwise the forecasts will be inefficient, 
#as there is more information in the data that can be exploited.
  #they are unrelated to the predictor variables; otherwise there would
    #be more information that should be included in the systematic part of the model.


#5.2 the least squares estimation

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
  
df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)


cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

#some useful preditors
  # trend variable
    # A trend variable can be specified in the tslm() function using the trend predictor.
  # dummy variable
  
#


