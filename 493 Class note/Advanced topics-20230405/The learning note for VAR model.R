library("ggplot2")
library("fpp2")
library("glmnet")
library("tidyr")
library("lmtest")
library("boot")
library("forecast")
library("readr")
library("ggfortify")
library("tseries")
library("urca")
library("readxl")
library("lubridate")
library("cansim")       
library("OECD")        
library("WDI")          
library("fredr")        
library("tsbox")
library("RColorBrewer")
library("wesanderson")
library("writexl")
library("readr")
library("lubridate")
library("gridExtra")
library("vars")

install.packages(
  "vars"
)

########################################################################

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

########################################################################


#how many lags in the var? 
  #the lag lenght for var model may be determined using model selection criteria. 
  #for forecasting with Var model, we perfer to using the BIC.


#VARselect() for selecting the number of lags p using different information criteria
#VAR() for fitting VAR models
#Serial.test() for theresiduals test. 



# read quarterly data
data <- read.csv("493 Class note/Advanced topics-20230405/us_macro_quarterly.csv", header = TRUE)

# get time series
gdpgr <- 400*diff(log(data$GDPC96)) 
#convert from a quarterly frequency to an annual frequency


tsprd <- data$GS10-data$TB3MS
#he difference between the 10-year Treasury bond yield (GS10) 
#the 3-month Treasury bill yield (TB3MS) for each observation in the "data" object.

tsprd <- tsprd[-1]
#set each variable to the lag one

# full sample
vardata0 <- ts(cbind(gdpgr,tsprd), start=c(1957,2), frequency=4)
vardata0 <- window(vardata0, start=c(1984,1), end=c(2012,4))


#plot the data
autoplot(vardata0, facets = TRUE, colour=TRUE) + 
  ylab("") + xlab("") + guides(colour="none")

#some note relate to the VARselect()

  #Syntax: VARselect(x, lag.max, type = "const")
#x: A matrix or data frame containing the time series data to be modeled. Each column of the data represents a separate variable in the VAR model.
#lag.max: The maximum lag order to consider when fitting the VAR model. This should be an integer value.
#type: The type of VAR model to fit, specified as a character string. T
  #The default value is "const", which specifies a model with a constant term. 
  #Other options include "none" (for a model with no intercept) and "trend" (for a model with a linear trend).


VARselect(vardata0, lag.max = 8, type="const")[["selection"]]


# estimate VAR(2)
model0 <- VAR(vardata0, p = 2, type = "const")

#what is serial.test? 

serial.test(model0, lags.pt = 10, type = "PT.asymptotic")
#the residuals is the white nose 


#Forecasting in VAR model

#在进行向量自回归（VAR）模型的预测时，有两种方法：
#递归法 (recursive manner) 
#直接法 (direct approach )

#递归法是指使用VAR模型，根据其他变量的预测值，逐步预测系统中每个变量的预测值。
  #递归法的优点是可以自动地反映模型的调整和数据更新，同时能够较好地反映出变量之间的相互作用。
  #递归法的缺点是计算量较大，特别是当时间序列的长度比较长时，计算复杂度会更高。

#直接法是指使用VAR模型，直接求解所有变量的预测值。
#直接法的优点是计算量较小，计算速度较快，尤其在时间序列比较长时优势更加明显。
#直接法的缺点是不能反映出变量之间的相互作用，因此对于变量之间的关联程度比较强的系统来说，
#直接法的预测精度可能会比递归法低。


fcast0 <- forecast(model0, h=2)
fcast0


autoplot(forecast(model0, h=10)) + xlab("")


#Granger causality
  #using the one time series past data to forecast the future value of the other time series
  #if a variable y1 is found to be helpful for prediciting another variable y2 and y1 is said to 
    #Granger_cause y2
  #The notion of Granger causality doesn't imply true causality, only implies forecasting ability. 



causality(model0,cause="tsprd")$Granger
causality(model0,cause="gdpgr")$Granger


# The recursive estimation

# The model estimated using recurisve estimation 
  #models estimated using an expanding window (that is, using all available data to estimate parameters)
  #If GDP is stale (no structural breaks), increasing the sample size reduces the variance of hte parpameter estimates.

#The rolling estimation
  #models estimated using a fixed windos (that is, using only last r observation to estimate parameters)
  #If GDP is unstable (Structural breaks), using earliest data may lead to biased parameter estimates and ofrecasts.



#####################################################################################################################

# simulate data
y1 <- arima.sim(list(ar = 0.5), n = 200)
autoplot(y1) + xlab("") + ylab("")

pred.rec <- matrix(rep(NA,450),150,3)
pred.rol <- matrix(rep(NA,450),150,3)


n.end <- 50
for(i in 1:150){
  # recursive forecast
  tmp0 <- 1
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rec[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rec[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rec[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
  # rolling forecast
  tmp0 <- i
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rol[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rol[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rol[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
}

######
cbind(pred.rec[,1],pred.rol[,1])[1:5,]


######
fcst.all <- ts(
  cbind(pred.rec[,2],pred.rec[,3],pred.rol[,3]), 
  start=n.end+1, 
  names=c("Actual","Recursive","Rolling")
)
autoplot(y1) + autolayer(fcst.all) + xlab("") + ylab("") +
  theme(legend.position = "bottom")


#####
fit.rec <- pred.rec[,2] - pred.rec[,3]
fit.rol <- pred.rol[,2] - pred.rol[,3]
rmse.rec <- sqrt(mean(fit.rec^2, na.rm=TRUE))
rmse.rol <- sqrt(mean(fit.rol^2, na.rm=TRUE))
cbind(rmse.rec,rmse.rol)





############ example 2 the one structure break 

tc <- rep(0,200)
tc[50] <-  1
ls <- filter(tc, filter=1, method="recursive")
autoplot(ls) + xlab("") + ylab("")

##
y1 <- 4*ls + arima.sim(list(ar = 0.5), n = 200)
autoplot(y1) + xlab("") + ylab("")

##
# plot forecasts
fcst.all <- ts(
  cbind(pred.rec[,2],pred.rec[,3],pred.rol[,3]), 
  start=n.end+1, 
  names=c("Actual","Recursive","Rolling")
)
autoplot(y1) + autolayer(fcst.all) + xlab("") + ylab("") +
  theme(legend.position = "bottom")

#
# display rmse
fit.rec <- pred.rec[,2] - pred.rec[,3]
fit.rol <- pred.rol[,2] - pred.rol[,3]
rmse.rec <- sqrt(mean(fit.rec^2, na.rm=TRUE))
rmse.rol <- sqrt(mean(fit.rol^2, na.rm=TRUE))
cbind(rmse.rec,rmse.rol)


##### so the rolling is better for the macro data analysis 


###########################################################################################

#Given different forecasts, we may want to ask the following questions 
  #How good is particular set of forecasts? 
  #is one set of forecasts better than other one?
  #is it possible to get a better forecast as a combination of various forecast for the same variable ?


#The forecast should be unbiased, 
  #mean that the expect value of the forecast error should be equal to zero
  #The variance of the variable shoul be larger than the variance of a good forecast.


#The efficiency
  #forecast should be efficient, means that the optimal forecast error should be uncorrelatied
  #with available information at the time the forecast was made.
  # 在预测中，“有效性”指的是预测误差与预测时可用信息不相关。
  #这意味着，在预测时所用的信息已尽可能地考虑了所有相关信息，
  #而预测的误差只是由于随机和不可预测的因素造成的。

#The weak efficiency test against past prediction errors 

#弱有效性测试是指通过检验过去的预测误差是否与当前预测误差存在相关性，
#来评估预测模型的有效性。如果预测误差之间存在相关性，则说明预测模型没有完全利用过去的信息，
#因此需要进行改进

#Strong efficiency test againest other variables.

#强有效性测试则是指检验预测误差是否与其他变量存在相关性，
#以评估预测模型的有效性。如果预测误差与其他变量存在相关性，
#则说明预测模型没有考虑到所有相关的信息，也需要进行改进。


#这两种有效性测试都是重要的，因为它们能够帮助我们评估预测模型的性能和可靠性，找出可能存在的问题并进行改进。
#通过有效性测试，我们可以选择最优的预测模型，并进行更准确、可靠的预测，以帮助我们做出更好的决策。




# beer data
beer1 <- window(ausbeer, start=1992,end=c(2009,4))
autoplot(beer1) + xlab("") + ylab("Megalitres")


# end of training set, 2004Q4
n.end <- 2004.75 

# set matrix for storage, 20 obs in test set
pred <- matrix(rep(NA,80),20,4)
# loop
for(i in 1:20){
  tmp0 <- 1992
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(beer1,tmp0,tmp1)
  pred[i,1] <- window(beer1,tmp1+.25,tmp1+.25) # actual 
  # compute forecasts
  pred[i,2] <- meanf(tmp, h=1)$mean # mean forecast
  pred[i,3] <- snaive(tmp, h=1)$mean # seasonal last value
  pred[i,4] <- forecast(tslm(tmp~trend+season), h=1)$mean # trend + seasonal dummies
}


###########################################################################

fcast.all <- ts(
  pred, 
  start=2005, 
  frequency=4, 
  names=c("Actual","Mean","SNaive","STrend")
)
autoplot(fcast.all) + ylab("") + xlab("") +
  theme(legend.position = "bottom")

######################################################################################################################################################

beerfit1 <- pred[,1] - pred[,2]
e1 <- ts(beerfit1, start=2005, frequency=4)
# robust tests only need for h>1
test.eq <- tslm(e1~1)
coeftest(test.eq)


## ---------------------------------------------------------------------
beerfit1 <- pred[,1] - pred[,2]
e1 <- ts(beerfit1, start=2005, frequency=4)
# robust tests only need for h>1
test.eq <- tslm(e1~1)
coeftest(test.eq)


## ---------------------------------------------------------------------
# robust tests only need for h>1
test.eq <- Arima(e1,order=c(1,0,0))
coeftest(test.eq)


######################################################################################################################################################

dm.test()



# Load required packages
library("forecast")
install.packages("DMtest")
library("DMtest")

# Generate two time series forecasts
ts1 <- forecast(AirPassengers, h=12, level=c(95))
ts2 <- rwf(AirPassengers, h=12, level=c(95))

# Calculate DM test statistic
dm.test(ts1$mean, ts2$mean, alternative="two.sided", h=12, power=1, loss="quadratic")


##################################################

#when alternative forecasts are available, rather than selecting one of them we. can combine them.
#In the presence of a large number of alternative forecasts, a sinple average tend to work well.


#### The missing value
  #missing data can arise for many are the effects on forecasts depend on the specific context. 
  #in some situations, the missingness may be essentially random
    #(for example, someone may have forgotten to record the sales ﬁgures)

  #if the timing of the missing data is not informative for the forecasting problem, t
  #hen the missing values can be handled more easily


#### THe outliner
  #The outliers are obsevation that are very different from the majority of the observation in the time seties.
  #they may be error, or they may simply be unsual
  #all of the methods we have considered will not work well if there are extreme outliers in the data
  #we may wish to replace them with missing values, or with an estimate that is more consistent with the majority of the data



####################################################################################
#however
  #simply replacing outliers without thinking about why they have occurred is a dangerous practice
  #hey may provide useful information about the process that produced the data, and which should be taken into account when forecasting
  #if we are willing to assume that the outliers are genuinely errors, or that they won’t occur in the forecasting period, 
    #then replacing them can make the forecasting task easie



######################################################################################

#NBER recession dates

#conclustion: forecasting is hard 
  #we learned that an AR(2) model for US real GDP growth does not work well in recessions (biased forecasts)
  #a good predictive model for US recessions is needed to improve forecasts of real GDP growth (for example, a Probit model with more predictors)



  #Chauvet and Potter (2013) show that this can be done (the models are complicated and outside the scope of this course)