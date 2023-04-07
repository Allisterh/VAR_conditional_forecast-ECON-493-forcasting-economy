#495. 
#Tie.Ma. 
#1537905. 


#Step one Lode the all package that necessary or not? 
#yes, I just copy it around, so I did not need to check around.

####package#########################################################################################################
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
#####################Introduction########################################################################################################
#This code has the data analysis and forecasting for the canadian housing reslare data published by the CREA
#I Did realize this code is too much and too massy to read, here is the index.

#Index

#1.the data process
  #1.1 check the stationary

#2.the data analysis and forecast (including the Covid-shock)
  #2.1 all data ARIMA model
  #2.2 After 2008 ARIMA model
  #2.3 before and after covid ARIMA model (2016-2023)
  #2.4 After covid model(from 2022 - 2023)
  #2.5 some conclution and help?

#3 The data analysis and forecast (without the Covid-shock)

#4 Compare the model from #2 and #3
#5 Conditional forecast (still fixing)
#6 additional part just for testing my idea. 

#7 The Var model
#working on it now

#note: have a good day :)
#to be honest, I have no idea what I am doing half of time.




#####################1.import the data########################################################################################

#1 The bacis data processing
########################import the data
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

#transfore the data in to ts
Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)

#some plot
autoplot(Can_month_housing_sell.ts)
checkresiduals(Can_month_housing_sell.ts)
autoplot(decompose(Can_month_housing_sell.ts))
pacf(Can_month_housing_sell.ts)

##########################1.1 basics data analysis
#####################1.1 The data exam#####################
#Are data is stationary? 
adf.test(Can_month_housing_sell.ts)
kpss.test(Can_month_housing_sell.ts)
#all test suggest this time series is not stationary
#####################stationary data################################################################
Can_month_housing_sell_df.ts <- diff(Can_month_housing_sell.ts, lag = 1)
autoplot(Can_month_housing_sell_df.ts)

adf.test(Can_month_housing_sell_df.ts)
kpss.test(Can_month_housing_sell_df.ts)
# The union root test tell the data is stationary.


#############################################################################################################################
#2.the data analysis and forecast (including the COVID-shock)################################################################
#############################################################################################################################
#########################2.1 all data ARIMA model############################################################

### This code force auto,arima to search 15625 combinations of ARIMA
    #from ARIMA(1,1,1)(1,1,1) go to ARIMA (5,5,5)(5,5,5)
    #it will consume a huge amount of computing point, it took me 36s

all_data_model_1 <- auto.arima(Can_month_housing_sell.ts, 
                                approximation = FALSE, 
                                parallel = TRUE, 
                                stepwise = FALSE,
                                max.Q = 5, max.P = 5, max.D = 5,
                                max.d = 5, max.p = 5, max.q = 5)
print(all_data_model_1)     
#ARIMA(2,1,0) 

### so, what dos the default auto-arima will give me and how long it take?

all_data_model_2 <- auto.arima(Can_month_housing_sell.ts)
print(all_data_model_2)
#ARIMA(0,1,1) random walk with draft.
#0.025s and ARIMA(0,1,1)

#######################so, which one is better? 

######compare the AIC?
AIC(all_data_model_2)
#AIC = 3583.642
AIC(all_data_model_1)
#AIC = 3582.467
#while, they both seem good almost idendical.
#and the data seem to be randomly walk...#########################################################################
#########################2.1.1 all data ARIMA model forecasting################################################################################
#ARIMA(2,1,0)
print(all_data_model_1)
#check the model
checkresiduals(all_data_model_1)
Pacf(all_data_model_1$residuals)
#it look goooood

########### forecasting time
all_data_forecast_1 <- forecast(all_data_model_1, h = 6)
all_data_forecast_2 <- forecast(all_data_model_2, h = 6)
autoplot(all_data_forecast)
autoplot(all_data_forecast_2)

#############################################################################################################################
#########################2.2. while if we only forecast the data after 2008?##########################################################################################

######################### after_2008_forecasting.
#constructed model 2: what if we do not including the data from before the 2008 the economic crisis?
after_2008_forecasting_data <- window(Can_month_housing_sell.ts, start= c(2009,7), end= c(2023,2))

#plot the graphy
autoplot(after_2008_forecasting_data)

#the data look coool, now it is the time to play with data.
after_2008_forecasting_model_1 <- auto.arima(after_2008_forecasting_data, 
                               approximation = FALSE, 
                               parallel = TRUE, 
                               stepwise = FALSE,
                               max.Q = 5, max.P = 5, max.D = 5,
                               max.d = 5, max.p = 5, max.q = 5)
#ARIMA(2,1,0)
print(after_2008_forecasting_model_1)

after_2008_forecasting_model_2 <- auto.arima(after_2008_forecasting_data)
print(after_2008_forecasting_model_2)
#ARIMA(0,1,1) still random walk.
#########################2.2.1#which one is better?############################################################
AIC(after_2008_forecasting_model_2)
#3041.171
AIC(after_2008_forecasting_model_1)
#3038.807
#############################################################################################################################
#########################2.2.2forecasting time####################################################################################################
after_2008_forecasting_model_forecast <- forecast(after_2008_forecasting_model, h = 5)
autoplot(after_2008_forecasting_model_forecast)




##############################################################################v#############v

######### compare those two model: the one with full data and the data after 2008? 
########### the graphic
# Plot the time series data
#autoplot(Can_month_housing_sell.ts)
# Add the forecast from the after_2008_forecasting_model
#autoplot(forecast(after_2008_forecasting_model, h = 20)$mean, col = "blue")
# Add the forecast from the all_data_forecast model
#lines(forecast(all_data_model_1, h = 20)$mean , col = "red")
# Add a legend
#########################2.3: what if we only including the data from 2016 - 2023 alone? ####################################
####################### process the data. 
The_before_during_after_Covid_model.data<-window(Can_month_housing_sell.ts, start= c(2016,1), end= c(2023,1))

#autoplot(The_before_during_after_Covid_model.data)

#checkresiduals(The_before_during_after_Covid_model.data)
#diff_The_before_during_after_Covid_model.data <- diff(The_before_during_after_Covid_model.data, lag = 1)
#adf.test(diff_The_before_during_after_Covid_model.data)
#yep, the data is staionary. 


#the data look coool, now it is the time to play with data.
#autoplot(The_before_during_after_Covid_model.data)

The_before_during_after_Covid_model_1 <- auto.arima(The_before_during_after_Covid_model.data, 
                                             approximation = FALSE, 
                                             parallel = T, 
                                             d =1,
                                             stepwise = FALSE,
                                             max.Q = 5, max.P = 5, max.D = 5,
                                             max.d = 5, max.p = 5, max.q = 5)
print(The_before_during_after_Covid_model_1)
#ARIMA(2,1,0)



The_before_during_after_Covid_model_2 <- auto.arima(The_before_during_after_Covid_model.data, d=1)
print(The_before_during_after_Covid_model_2)
#ARIMA(0,1,1) the random walk drift.

The_before_during_after_Covid_model_2_forecast <- forecast(The_before_during_after_Covid_model_1, h = 5)
autoplot(The_before_during_after_Covid_model_2)
#########################2.3.1:Compare the model.####################################################################################################
AIC(The_before_during_after_Covid_model_1)
#1413.121
AIC(The_before_during_after_Covid_model_2)
#1413.473
checkresiduals(The_before_during_after_Covid_model)
(The_before_during_after_Covid_model)

#ARIMA(2,1,0)
#############################################################################################################################
#########################2.4 how about the data from 2020 alone?#############################################################
#########################2.4.1 process the data ##############################################################################################################
The_2022_along.data<-window(Can_month_housing_sell.ts, start= c(2020,1), end= c(2023,2))
The_2022_along_model_1<- auto.arima(The_2022_along.data, 
                                                    approximation = FALSE, 
                                                    parallel = T, 
                                                    stepwise = FALSE,
                                                    max.Q = 5, max.P = 5, max.D = 5,
                                                    max.d = 5, max.p = 5, max.q = 5)

print(The_2022_along_model_1)
#ARIMA(1,1,0)

The_2022_along.data_model_2 <- auto.arima(The_2022_along.data)
print(The_2022_along.data_model_2)
#ARIMA(0,1,0) with drift 
#########################2.4.2 compare the model##################################################################################
AIC(The_2022_along_model_1)
AIC(The_2022_along.data_model_2)


#############################################################################################################################
#########################2.5 some conclution?################################################################################
#########################2.5.1 plot the all forecast outcome#######################################################
# Create a matrix to store the forecasts
forecasts <- matrix(rep(NA,24),6,4)

# Generate forecasts for the next 6 periods using each model
forecasts[,1] <- forecast(The_2022_along_model_1,h=6)$mean
forecasts[,2] <- forecast(The_before_during_after_Covid_model_1,h=6)$mean
forecasts[,3] <- forecast(after_2008_forecasting_model_1,h=6)$mean
forecasts[,4] <- forecast(all_data_model_1,h=6)$mean


# Plot the forecasts in one graph
plot.ts(forecasts, main = "Forecast Graph for 4 ARIMA Models", xlab = "Period", ylab = "Forecasted Values", col = 1:4)





####################let's compare ARIMA (2,1,0) with ARIMA(0,1,1) to see which one is btter .


#### set up the test enviroment 
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

Can_month_housing_sell_without_covid_shock.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2017, 12), frequency = 12)
#transfor the data to the ts


###### set up model

diff_Can_month_housing_sell.ts <- diff(Can_month_housing_sell.ts)

#test_model_one<- auto.arima(Can_month_housing_sell.ts, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)
#print(test_model_one)
the_model_without_covid_shock <- auto.arima(Can_month_housing_sell.ts)
#ARIMA(2,0,2)(0,0,1)[12] with zero mean 
#note: both give the same outcome.##############################################################

#############################################################################################################################
#Part 3: The model construction without the covid shock
#############################################################################################################################
#########################3.1 set up the test environment######################################################################

Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

Can_month_housing_sell_without_covid_shock.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2019, 12), frequency = 12)
after_2008_forecasting_without_covid_shock_data <- window(Can_month_housing_sell.ts, start= c(2009,6), end= c(2019, 12))
The_before_during_after_Covid_without_covid_shock_data<-window(Can_month_housing_sell.ts, start= c(2016,1), end= c(2017, 12))
##################################################################
#########################3.2 modeling time###################################################################################################################
####model time
test_model_one<- auto.arima(Can_month_housing_sell_without_covid_shock.ts, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)
test_model_one_after_2008_forecasting_without_covid_shock<- auto.arima(after_2008_forecasting_without_covid_shock_data, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)
test_model_one_The_before_during_after_Covid_without_covid_shock_data<- auto.arima(The_before_during_after_Covid_without_covid_shock_data, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)


dm.test(test_model_one, test_model_one_after_2008_forecasting_without_covid_shock, h = 4)


print(test_model_one)
#ARIMA(2,1,1)(0,0,2)[12] 
print(test_model_one_after_2008_forecasting_without_covid_shock)
#ARIMA(2,1,1)(1,0,1)[12]

wee <- auto.arima(The_before_during_after_Covid_without_covid_shock_data)
################################################################################
#########################3.3 plot the graphy###############################################################################################################################################
autoplot(Can_month_housing_sell_without_covid_shock.ts)
autoplot(after_2008_forecasting_without_covid_shock_data)

#####3.
print(test_model_one)
print(test_model_one_after_2008_forecasting_without_covid_shock)
#ARIMA(3,1,1)(0,0,1)[12]
print(test_model_one_The_before_during_after_Covid_without_covid_shock_data)##############################################################################
#############################################################################################################################


##############################################################################################################################
#Part 4:compare the three model for the entire data
#########################4.1 the model list##################################################################################
#ARIMA (2,1,0)
#ARIMA (0.1.1)
#ARIMA ARIMA(2,1,2)(0,0,1)[12] with zero mean 
#ARIMA(3,1,1)(0,0,1)[12]
#ARIMA(2,1,2)(1,0,1)[12]
#########################4.2 fit all the model into ARIMA ####################################################################

#first, compare the AICc of the model for overall proferement


Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)

# Fit the ARIMA(2,1,0) model
model1 <- arima(Can_month_housing_sell.ts, order=c(2,1,0))

# Fit the ARIMA(0,1,1) model
model2 <- arima(Can_month_housing_sell.ts, order=c(0,1,1))

# Fit the ARIMA(2,1,2)(0,0,2)[12] model with zero mean
model3 <- arima(Can_month_housing_sell.ts, order=c(2,1,2), seasonal=list(order=c(0,0,2), period=12))

# Fit the ARIMA(2,1,1)(1,0,1)[12] model
model4 <- arima(Can_month_housing_sell.ts, order=c(2,1,1), seasonal=list(order=c(1,0,1), period=12))

#ARIMA(2,1,2)(0,0,1)[12] 
model5 <- arima(Can_month_housing_sell.ts, order=c(2,1,2), seasonal=list(order=c(0,0,1), period=12))


#########################4.3 compare all model in both AIC and BIC############################

#AIC matrix
AIC_and_BIC_Matrix <- matrix (ncol = 2, nrow = 5)
colnames(AIC_and_BIC_Matrix) <-c("AIC","BIC")
rownames(AIC_and_BIC_Matrix) <-c("ARIMA(2,1,0)", "ARIMA(0,1,1)", "ARIMA(2,1,2)(0,0,2)[12]", "ARIMA(2,1,2)(1,0,1)[12]", "ARIMA(2,1,2)(0,0,1)[12]")
AIC_and_BIC_Matrix[1,1] <- AIC(model1)
AIC_and_BIC_Matrix[2,1] <- AIC(model2)
AIC_and_BIC_Matrix[3,1] <- AIC(model3)
AIC_and_BIC_Matrix[4,1] <- AIC(model4)
AIC_and_BIC_Matrix[5,1] <- AIC(model5)
AIC_and_BIC_Matrix[1,2] <- BIC(model1)
AIC_and_BIC_Matrix[2,2] <- BIC(model2)
AIC_and_BIC_Matrix[3,2] <- BIC(model3)
AIC_and_BIC_Matrix[4,2] <- BIC(model4)
AIC_and_BIC_Matrix[5,2] <- BIC(model5)

print(AIC_and_BIC_Matrix)


#########################4.4: The cross validation##############################################################################

# end of training set
n.end <- 2017

# The number of observation.
n_test <- 12 * (2019 - 2018)

n_models <- 5  #the number of model

autoplot(Can_month_housing_sell.ts)

pred <- matrix(rep(NA, n_test * (n_models + 1)), nrow=n_test, ncol=(n_models + 1))  # Initialize the prediction matrix | 初始化预测矩阵

# Loop through the test observations | 遍历测试观测值
for (i in 1:n_test) {
  # Set the window for the training data | 设置训练数据窗口
  tmp0 <- 2007
  tmp1 <- n.end + (i - 1) * (1/12)
  tmp <- window(Can_month_housing_sell.ts, start=tmp0, end=tmp1)
  
  # Actual value | 实际值
  pred[i, 1] <- window(Can_month_housing_sell.ts, start=tmp1 + (1/12), end=tmp1 + (1/12))
  
  # Fit the ARIMA models on the rolling training data | 在滚动训练数据上拟合ARIMA模型
  model1_train <- arima(tmp, order=c(2,1,0))
  model2_train <- arima(tmp, order=c(0,1,1))
  model3_train <- arima(tmp, order=c(2,1,2), seasonal=list(order=c(0,0,2), period=12))
  model4_train <- arima(tmp, order=c(2,1,1), seasonal=list(order=c(1,0,1), period=12))
  model5_train <- arima(tmp, order=c(2,1,2), seasonal=list(order=c(0,0,1), period=12))
  
  # Forecast one step ahead | 预测提前一步
  pred[i, 2] <- forecast(model1_train, h=1)$mean
  pred[i, 3] <- forecast(model2_train, h=1)$mean
  pred[i, 4] <- forecast(model3_train, h=1)$mean
  pred[i, 5] <- forecast(model4_train, h=1)$mean
  pred[i, 6] <- forecast(model5_train, h=1)$mean
}

# Calculate error metrics for each model | 计算每个模型的误差指标
errors <- (pred[, -1] - pred[, 1])
me <- colMeans(errors)
rmse <- sqrt(colMeans(errors^2))
mae <- colMeans(abs(errors))
mpe <- colMeans((errors / pred[, 1]) * 100)
mape <- colMeans(abs((errors / pred[, 1]) * 100))

# Print error metrics for each model | 打印每个模型的误差指标
cat("ME: ", me, "\n")
cat("RMSE: ", rmse, "\n")
cat("MAE: ", mae, "\n")
cat("MPE: ", mpe, "\n")
cat("MAPE: ", mape, "\n")
    
# Create a data frame to store the error metrics for each model | 为每个模型创建一个存储误差指标的数据框
error_metrics <- data.frame(
  Model = c("ARIMA(2,1,0)", "ARIMA(0,1,1)", "ARIMA(2,1,2)(0,0,2)[12]", "ARIMA(2,1,1)(1,0,1)[12]", "ARIMA(2,1,2)(0,0,1)[12]"),
  ME = me,
  RMSE = rmse,
  MAE = mae,
  MPE = mpe,
  MAPE = mape
)

# Print the table with error metrics for each model | 打印每个模型的误差指标表格
print(error_metrics)





#########################4.4.2: the underhandedness test: Variance##############################################################################

par(mfrow=c(3,2))
plot(residuals(model1), main="Model 1 Residuals", ylab="Residuals")
plot(residuals(model2), main="Model 2 Residuals", ylab="Residuals")
plot(residuals(model3), main="Model 3 Residuals", ylab="Residuals")
plot(residuals(model4), main="Model 4 Residuals", ylab="Residuals")
plot(residuals(model5), main="Model 5 Residuals", ylab="Residuals")



rmse_values <- sqrt(mse_values)
names(rmse_values) <- c("model1", "model2", "model3", "model4", "model5")
rmse_values


#Part2: play around the housing sell data with other data  #######################
##############################################################################################################################











#2.2 The data base import 
everyhingCA_stationary_raw <- read_csv("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/LCDMA_February_2023/CAN_MD_asd.xlsx")
#View(everyhingCA.raw)

everyhingCA_raw<- read_csv("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/LCDMA_February_2023/CAN_MD.csv") 
#View(everyhingCA_raw)

######## that is too much data there....


##########The relationship between the housing sell with bank rate?
CA_bank_rate_raw <- "v122550"
CA_bank_rate.st <- get_cansim_vector(CA_bank_rate_raw, start_time = "2006-12-01", end_time = "2023-02-01")
CA_bank_rate_year.st <- year(CA_bank_rate.st$REF_DATE[1])
CA_bank_rate_month.st <- month(CA_bank_rate.st$REF_DATE[1])

#transfer data to the time series time
c(CA_bank_rate_year.st, CA_bank_rate_month.st)
CA_bank_rate.ts <-ts(CA_bank_rate.st$VALUE, start = c(CA_bank_rate_year.st, CA_bank_rate_month.st), freq = 12)
CA_bank_rate_diff.ts <- diff(CA_bank_rate.ts, lag = 1)

#doing something interesting
housing_sell_plus_policy_rate_model<- auto.arima(Can_month_housing_sell.ts, d =1, 
                                                 xreg = CA_bank_rate_diff.ts, 
                                                 approximation = FALSE, parallel = T, 
                                                 stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,
                                                 max.d = 5, max.p = 5, max.q = 5)
#Regression with ARIMA(1,1,3)(0,0,1)[12] errors 

x <- 12
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- 0.0
}
new_bank_data <- print(values)

print(housing_sell_plus_policy_rate_model)
checkresiduals(housing_sell_plus_policy_rate_model)
evil_forcast <- forecast(housing_sell_plus_policy_rate_model, h = 12, xreg = new_bank_data)
autoplot(evil_forcast)

#it match with the relationship between the housing market and policy rate.


##################

################# The relationship between housing sell and GPD?
CA_GDP_raw <- "v65201483"
CA_GDP.st <- get_cansim_vector(CA_GDP_raw, start_time = "2005-12-01", end_time = "2022-12-01")
CA_GDP_year.st <- year(CA_GDP.st$REF_DATE[1])
CA_GDP_month.st <- month(CA_GDP.st$REF_DATE[1])
#transfer data to the time series time
c(CA_GDP_year.st, CA_GDP_month.st)
CA_GDP.ts <-ts(CA_GDP.st$VALUE, start = c(CA_GDP_year.st, CA_GDP_month.st), freq = 12)
plot(CA_GDP.ts)

###
CA_GDP_diff.ts <- diff(log(CA_GDP.ts))
plot(CA_GDP_log_diff.ts)
#adf.test(CA_GDP_log_diff.ts)
#The ADF test look good.
###

#create a new time series object using a subset of the data
Can_month_housing_sell_GDP_model.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2006,1), end = c(2022,12), frequency =12)
plot(Can_month_housing_sell_GDP_model.ts)

housing_sell_plus_GDP_model<- auto.arima(Can_month_housing_sell_GDP_model.ts, xreg= CA_GDP_diff.ts,
                                         approximation = FALSE, parallel = T, stepwise = FALSE, 
                                         max.d = 5, max.p = 5, max.q = 5)
print(housing_sell_plus_GDP_model)

#display the resulting time series
Can_month_housing_sell_GDP_model.ts
adf.test(Can_month_housing_sell_GDP_model.ts)
###


x <- 20
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- 0.01
}
GDP_new_data <- print(values)

housing_sell_plus_GDP_model_forecast <- forecast(PI = T, housing_sell_plus_GDP_model, h = 36, xreg = GDP_new_data)
autoplot(housing_sell_plus_GDP_model_forecast)
######### when the inflation high, the housing sell drop.


#############
#How about the housing sell with the average hourly wage rate?


fix_1<- nnetar(Can_month_housing_sell_GDP_model.ts, xreg = CA_GDP_diff.ts)
fix_forecast <- forecast(fix_1,h=20, xreg =  GDP_new_data)
autoplot(fix_1, PI=f, h = 30)

#THE 
fix_2

##############
#How about the housing sell with the average hourily age? 
hourly_average_wage_raw <- "v2132579"
hourly_average_wag.st <- get_cansim_vector(hourly_average_wage_raw, start_time = "2006-12-01", end_time = "2022-12-01")
hourly_average_wage_year.st <- year(hourly_average_wag.st$REF_DATE[1])
hourly_average_wage_month.st <- month(hourly_average_wag.st$REF_DATE[1])
#transfer data to the time series time
c(hourly_average_wage_year.st, hourly_average_wage_month.st)
hourly_average_wage.ts <-ts(hourly_average_wag.st$VALUE, start = c(hourly_average_wage_year.st, hourly_average_wage_month.st), freq = 12)
plot(hourly_average_wage.ts)

log_hourly_average_wage.ts <- diff(log(hourly_average_wage.ts))

checkresiduals(log_hourly_average_wage.ts)
adf.test(log_hourly_average_wage.ts *100)

#checkresiduals(diff_hourly_average_wage.ts)
#The data got strong seasonality
#do other seasonal different. 
#Check the staionarlity
##statioanry check
adf.test(diff_hourly_average_wage.ts)
# p-value smaller than printed p-value
kpss.test(seasonaldiff_diff_hourly_average_wage.ts)
######
#The data is stationary.
#we can keep going!
Can_month_housing_sell_GDP_model.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007,1), end = c(2022,12), frequency =12)

housing_sell_plus_hourly_wage_model<- auto.arima(Can_month_housing_sell_GDP_model.ts, 
                                                 xreg=(log_hourly_average_wage.ts*100), 
                                                 approximation = FALSE, parallel = T, stepwise = FALSE,
                                                 max.P = 5, max.Q = 5, max.D = 5,
                                                 max.d = 5, max.p = 5, max.q = 5)
#Regression with ARIMA(1,1,3) errors 
summary(housing_sell_plus_hourly_wage_model)


x <- 36
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- 20
  }
The_wage_incerase_rata <- print(values)

housing_sell_plus_hourly_wage_model_forecast <- forecast( housing_sell_plus_hourly_wage_model, h = 36, xreg = The_wage_incerase_rata)
autoplot(housing_sell_plus_hourly_wage_model_forecast)


###### how about the housing sell with the despotiable income? 
# Canadian disposable income, quarterly, seasonally adjusted
CA_disable_income.raw <- "v62305869"
CA_disable_income.st <- get_cansim_vector(CA_disable_income.raw, start_time = "2007-01-01", end_time = "2022-12-01")
CA_disable_income_year.st <- year(CA_disable_income.st$REF_DATE[1])
CA_disable_income_month.st <- month(CA_disable_income.st$REF_DATE[1])
#transfer data to the time series time
c(CA_disable_income_year.st, CA_disable_income_month.st)
CA_disable_income.ts <-ts(CA_disable_income.st$VALUE, start = c(CA_disable_income_year.st, CA_disable_income_month.st), freq = 4)
autoplot(CA_disable_income.ts)

#####stationary data

diff_log_CA_disable_income.ts <- diff(log(CA_disable_income.ts), lag = 1)
adf.test(diff_log_CA_disable_income.ts)
checkresiduals(diff_log_CA_disable_income.ts)

#### now the data is stationary, the next step is turn the housing sell data into quarternally data

#start at 2007q2
Can_month_housing_sell_quarterly.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 4), end = c(2022, 12), frequency = 12)

# Convert to zoo object for easy aggregation
monthly_data_zoo <- zoo(Can_month_housing_sell_quarterly.ts, order.by = index(Can_month_housing_sell_quarterly.ts))

# Aggregate to quarterly frequency by taking the average of every three consecutive months
Can_month_housing_sell_quarterly_data <- aggregate(monthly_data_zoo, as.yearqtr, mean)

### now its the quarterly data .

housing_sell_plus_disposable_income_model<- auto.arima(Can_month_housing_sell_quarterly_data, 
                                                 xreg=(diff_log_CA_disable_income.ts*100), 
                                                 approximation = FALSE, parallel = T, stepwise = FALSE,
                                                 max.P = 5, max.Q = 5, max.D = 5,
                                                 max.d = 5, max.p = 5, max.q = 5)
print(housing_sell_plus_disposable_income_model)

#Three different situion on


x <- 6
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- -2
}
The_dispoable_income_change_rate <- print(values)

housing_sell_plus_disposable_income_model_forecast <- forecast( housing_sell_plus_hourly_wage_model, h = 6, xreg = The_dispoable_income_change_rate)
autoplot(housing_sell_plus_disposable_income_model_forecast)

########## how is the relationship between the new housing start.

## import the data. 

The_housing_start.raw <- "v52300157"
The_housing_start.st <- get_cansim_vector(The_housing_start.raw, start_time = "2007-01-01", end_time = "2022-12-01")
The_housing_start_year.st <- year(The_housing_start.st$REF_DATE[1])
The_housing_start_month.st <- month(The_housing_start.st$REF_DATE[1])
#transfer data to the time series time
c(The_housing_start_year.st, The_housing_start_month.st)
The_housing_start.ts <-ts(The_housing_start.st$VALUE, start = c(The_housing_start_year.st, The_housing_start_month.st), freq = 12)
autoplot(The_housing_start.ts)

adf.test(diff(log(The_housing_start.ts)))
#one different is enough. 

diff_The_housing_start.ts <- (diff(log(The_housing_start.ts)))
Can_month_housing_sell_evil.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 2), end = c(2022, 12), frequency = 12)

housing_sell_plus_The_housing_start_model<- auto.arima(Can_month_housing_sell_evil.ts, 
                                                       xreg=(diff_The_housing_start.ts), 
                                                       approximation = FALSE, parallel = T, stepwise = FALSE,
                                                       max.P = 5, max.Q = 5, max.D = 5,
                                                       max.d = 5, max.p = 5, max.q = 5)
print(housing_sell_plus_The_housing_start_model)
#Regression with ARIMA(1,1,3) errors 


x <- 12
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- 0.2
}
new_housing_building_rate <- print(values)

housing_sell_plus_The_housing_start_model_forecast <- forecast(PI = T, housing_sell_plus_The_housing_start_model, h = 12, xreg = new_housing_building_rate)
autoplot(housing_sell_plus_The_housing_start_model_forecast)






































