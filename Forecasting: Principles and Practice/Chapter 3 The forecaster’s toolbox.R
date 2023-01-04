#Chapter 3 The forecaster’s toolbox\
#3.1 some simple forecasting method
  # average method
  #meanf(y, h)
    # y contains the time series
    # h is the forecast horizon'

#Native moethod
#Native (y,h)
#rwf(y, h) # Equivalent alternative
#Because a naïve forecast is optimal when data follow a random walk (see Section 8.1),
#these are also called random walk forecasts.

#Seasonal naïve method
#snaive(y, h)]
#A similar method is useful for highly seasonal data. In this case, we set each forecast to be equal 
#to the last observed value from the same season (e.g., the same month of the previous year). 

#Drift method
#A variation on the naïve method is to allow the forecasts to 
#increase or decrease over time, where the amount of change over time 


# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# 3.2 Transfomations and adjustments
#Calendar addjustments
#For the data that has seaoson variance 
#like a lot of shit 
#I guess it will be useful for 

#get the data set - the dire farm 
#cbind - bind the data toether,
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

#population 
#inflation
# math 
# the box-c0x
#the bias adjustment
#the back-transformed point forecast will not be the mean of the forecast distribution. 
#In fact, it will usually be the median of the forecast distribution

#the different between the simple bck-transformed orcast givand the mean is called the bis
#when we use the mean, rather than the median, we say the point forecasts have been bias-adjusted
#the bis adjustmnet is not down by defalut in the forcast package, if you want your forcasts to be
#mean rather than the median, use the argument biasedk= ture , when you s elect your box-cso tranfomation parameter.
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))


#3.3 risdual diagnostics 回归诊断
# The residuals in a times series model are what is left over after fitting a mdoe . for many times seris models 
# the residuals are equal to the difference vetween the observaation and the corresonding fitted value
# the the different berween the the fit value - the forcasting value

#Theresiduals are useful in checking whether a modeals ahas adequaterly captureed the informatiojn in the data
#A good foreccasting moethod will ield residuals with the following properties 
  #The residuals are uncorrelated 没关系的
    #if there are correlations between residuals, then there is information left in the residuals which should be used in the computing forcasts. 
  #the residuals have zero mean 
    #if the residuals have a mean other than zero, then the oecasting are zero.

#in addidtion to these essential properties, it is useful for the residuals to aslo have the following two properties 
  #THE RESDIUALS HAE CONSTANT variance
  #the residuals are noramlly distributed. 

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

#using different way to forecasting it 
fc <- rwf(goog200, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(goog200, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(goog200) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

#res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

#=# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
#> 
#>  Box-Pierce test
#> 
#> data:  res
#> X-squared = 11, df = 10, p-value = 0.4

Box.test(res,lag=10, fitdf=0, type="Lj")
#> 
#>  Box-Ljung test
#> 
#> data:  res
#> X-squared = 11, df = 10, p-value = 0.4
#> 
#using the fucking checkresiduals to heck the residuals

#evaluating forecasting accuarcy

#3.4 evaluating forecast accuaracy 
  #training and test set
#the two different type of error
  #the scale-dependent error
  # percetnage error
  #Scaled errors

beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=TRUE) +
  autolayer(beerfit2, series="Naïve", PI=TRUE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=TRUE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

#The accuaracy test
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)
#the lower the good

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))
 
#the fitness tesrt
googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)


#the times series cross variance
#Time series cross-validation is implemented with the tsCV()

e <- tsCV(goog200, rwf, drift=TRUE, h=1) 
sqrt(mean(e^2, na.rm=TRUE)) # Root-mean-square deviation
#) is a frequently used measure of the differences between values 
#(sample or population values) predicted by a model or an estimator and the values observed. 
#> [1] 6.233
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))
#> [1] 6.169
#> 
#> e <- tsCV(goog200, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

#3.7 practie

#1
#usnetelec using the population change 
(lambda <- BoxCox.lambda(usnetelec))
autoplot(BoxCox(usnetelec,lambda))

autoplot(usnetelec)
usele <-window(usnetelec)'

#enplanements
(lambda <- BoxCox.lambda(enplanements))
#> [1] 0.2654
autoplot(BoxCox(enplanements,lambda))

#WWWusage
#go for the native for one

wfc <- snaive(WWWusage)
wf2 <- naive(WWWusage)

checkresiduals(wfc)
checkresiduals(wf2)
#there is no different between those two 




