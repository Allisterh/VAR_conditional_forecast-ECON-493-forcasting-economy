#495. 
#Tie.Ma. 
#1537905. 
#this verson of code is write for the submit as the homework.
#there is an other verson of code called "some random data test" is more of the note.

###update note##########################################################################
#verso 2.0 update the var model 2023/04/07


#### loding package#########################################################################################################
library(ggplot2)
library(fpp2)
library(glmnet)
library(tidyr)
library(lmtest)
library(boot)
library(forecast)
library(readr)
library(ggfortify)
library(tseries)
library(urca)
library(readxl)
library(lubridate)
library(cansim)
library(OECD)
library(WDI)
library(fredr)
library(tsbox)
library(RColorBrewer)
library(wesanderson)
library(writexl)
library(gridExtra)
library(vars)



#1.import the data
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))

#transfer data to ts form
Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)

#1.1 data along graphy 

#1.1.1 the entire data graphy
autoplot(Can_month_housing_sell.ts)

#1.1.2 the seaonal plot
ggseasonplot(Can_month_housing_sell.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("number of home saled") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

#1.1.3 the seasonality graphy
ggsubseriesplot(Can_month_housing_sell.ts) +
  ylab("number of home saled") +
  ggtitle("Seasonal subseries plot: Can_month_housing_saless")

#2. ARIMA model

#2.1 how many difference we need to stationry data? 

adf.test(diff(Can_month_housing_sell.ts))
#one difference can stable the data already.


#2.2 using the entire data set to fit model 
#2.2.1 
all_data_model_1 <- auto.arima(Can_month_housing_sell.ts, 
                               approximation = FALSE, parallel = TRUE, stepwise = FALSE,
                               num.cores = 10, start.p = 0, start.q = 0, start.P = 0, 
                               start.Q = 0, max.Q = 5, max.P = 5, max.D = 5,
                               max.d = 5, max.p = 5, max.q = 5)
print(all_data_model_1)
#ARIMA(2,1,0)

#2,2,2 
all_data_model_2 <- auto.arima(Can_month_housing_sell.ts)
print(all_data_model_2)
#ARIMA(0,1,1)

#2.3.0 The data without 2008
#2.3.1 construed the data
after_2008_forecasting_data <- window(Can_month_housing_sell.ts, start= c(2009,7), end= c(2023,2))

#2.3.2 using the data to fit the modle
after_2008_forecasting_model_1 <- auto.arima(after_2008_forecasting_data, 
                                             approximation = FALSE, parallel = TRUE, stepwise = FALSE,
                                             num.cores = 10, start.p = 0, start.q = 0, start.P = 0, 
                                             start.Q = 0, max.Q = 5, max.P = 5, max.D = 5,
                                           max.d = 5, max.p = 5, max.q = 5)
print(after_2008_forecasting_model_1)
#ARIMA(2,1,0)

after_2008_forecasting_model_2 <- auto.arima(after_2008_forecasting_data)
print(after_2008_forecasting_model_2)
#ARIMA(0,1,1)

#2.4 using 2016 to 2023 data along to fit the model.

#2.4.1 construed the model.
The_before_during_after_Covid_model.data<-window(Can_month_housing_sell.ts, start= c(2016,1), end= c(2023,1))

#2.4.2 fit the model one
The_before_during_after_Covid_model_1 <- auto.arima(The_before_during_after_Covid_model.data, d = 1,
                                                    approximation = FALSE, parallel = TRUE, stepwise = FALSE,
                                                    num.cores = 10, start.p = 0, start.q = 0, start.P = 0, 
                                                    start.Q = 0, max.Q = 5, max.P = 5, max.D = 5,
                                                    max.d = 5, max.p = 5, max.q = 5)
print(The_before_during_after_Covid_model_1)
#ARIMA(2,1,0)

#2.4.3 fit the model two
The_before_during_after_Covid_model_2 <- auto.arima(The_before_during_after_Covid_model.data, d=1)
print(The_before_during_after_Covid_model_2)
#ARIMA(0,1,1) the random walk drift.

#2.5 using the data without covid shock to fit the model. 

#2.5.1 constructed the data.
Can_month_housing_sell_without_covid_shock.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2019, 1), frequency = 12)
after_2008_forecasting_without_covid_shock_data <- window(Can_month_housing_sell.ts, start= c(2009,6), end= c(2019, 1))

#2.5.2 using the model to fit hte data. 
entil_data_without_covid_shock_1<- auto.arima(Can_month_housing_sell_without_covid_shock.ts, 
                            approximation = FALSE, parallel = TRUE, stepwise = FALSE,
                            num.cores = 10, start.p = 0, start.q = 0, start.P = 0, 
                            start.Q = 0, max.Q = 5, max.P = 5, max.D = 5,
                            max.d = 5, max.p = 5, max.q = 5)

entil_data_without_covid_shock_2 <- auto.arima(Can_month_housing_sell_without_covid_shock.ts)

test_model_one_after_2008_forecasting_without_covid_shock_1 <- auto.arima(after_2008_forecasting_without_covid_shock_data,
                                                                        approximation = FALSE, parallel = TRUE, stepwise = FALSE,
                                                                        num.cores = 10, start.p = 0, start.q = 0, start.P = 0, 
                                                                        start.Q = 0, max.Q = 5, max.P = 5, max.D = 5,
                                                                        max.d = 5, max.p = 5, max.q = 5)

test_model_one_after_2008_forecasting_without_covid_shock_2 <- auto.arima(after_2008_forecasting_without_covid_shock_data)



print(entil_data_without_covid_shock_1)
#ARIMA(2,1,2)(0,0,1)[12]

print(entil_data_without_covid_shock_2)
#ARIMA(2,1,2)(0,0,1)[12] 

print(test_model_one_after_2008_forecasting_without_covid_shock_1)
#ARIMA(0,1,1)

print(test_model_one_after_2008_forecasting_without_covid_shock_2)
#ARIMA(0,1,0) random work




################################################################################
#########################3.3 plot the graphy###############################################################################################################################################
autoplot(Can_month_housing_sell_without_covid_shock.ts)
autoplot(after_2008_forecasting_without_covid_shock_data)

#####3.
print(test_model_one)
print(test_model_one_after_2008_forecasting_without_covid_shock)
#ARIMA(3,1,1)(0,0,1)[12]
print(test_model_one_The_before_during_after_Covid_without_covid_shock_data)




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
model1 <- Arima(Can_month_housing_sell.ts, order=c(2,1,0))

# Fit the ARIMA(0,1,1) model
model2 <- Arima(Can_month_housing_sell.ts, order=c(0,1,1))

# Fit the ARIMA(2,1,2)(0,0,2)[12] model with zero mean
model3 <- Arima(Can_month_housing_sell.ts, order=c(2,1,2), seasonal=list(order=c(0,0,2), period=12))

# Fit the ARIMA(2,1,1)(1,0,1)[12] model
model4 <- Arima(Can_month_housing_sell.ts, order=c(2,1,1), seasonal=list(order=c(1,0,1), period=12))

#ARIMA(2,1,2)(0,0,1)[12] 
model5 <- Arima(Can_month_housing_sell.ts, order=c(2,1,2), seasonal=list(order=c(0,0,1), period=12))


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

library(forecast)  # Load the forecast package | 加载预测包

# End of training set | 训练集结束
n.end <- 2019

# Set matrix for storage, assuming monthly data | 设置存储矩阵，假设月度数据
n_test <- 12 * (2023 - 2019)  # Number of test observations | 测试观测值数量
n_models <- 5  # Number of models | 模型数量
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


# Print error metrics for each model
cat("ME: ", me, "\n")
cat("RMSE: ", rmse, "\n")
cat("MAE: ", mae, "\n")
cat("MPE: ", mpe, "\n")
cat("MAPE: ", mape, "\n")

# Create a data frame to store the error metrics for each

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



#########################4.4.1: the underhandedness test: mean.   #############################################################################
# Calculate mean and variance of errors for each model | 计算每个模型误差的均值和方差
mean_errors <- colMeans(errors)
var_errors <- apply(errors, 2, var)

model_one_error <- errors[, 1]
pred[, 1]


the_unbiasednedd_of_mean <- lm(pred[, 1] ~ errors[, 3])
coeftest(the_unbiasednedd_of_mean)


dm.test(pred[, 1], pred[, 2], h = 1, power =2 )


#the unbiasedness of the mean forecast.



# Create a data frame to store the mean and variance of errors for each model | 为每个模型创建一个存储误差均值和方差的数据框
unbiasedness_test <- data.frame(
  Model = c("ARIMA(2,1,0)", "ARIMA(0,1,1)", "ARIMA(2,1,2)(0,0,2)[12]", "ARIMA(2,1,1)(1,0,1)[12]", "ARIMA(2,1,2)(0,0,1)[12]"),
  Mean = mean_errors,
  Variance = var_errors
)

# Print the table with mean and variance of errors for each model | 打印每个模型的误差均值和方差表格
print(unbiasedness_test)


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



#######################################################################################################################################
#5.0 The Var model
#Var data collection
Evil.begain <- "2007/01/01"
Evil.end <- "2022/12/31"

#5.1 input the data################################################################################

#The GDP data Canada [11124]; Seasonally adjusted at annual rates; 2012 constant prices; All industries
CA_GDP_raw <- "v65201483"
CA_GDP.st <- get_cansim_vector(CA_GDP_raw, start_time = Evil.begain, end_time = Evil.end)
CA_GDP_year.st <- year(CA_GDP.st$REF_DATE[1])
CA_GDP_month.st <- month(CA_GDP.st$REF_DATE[1])
#transfer data to the time series time
c(CA_GDP_year.st, CA_GDP_month.st)
CA_GDP.ts <-ts(CA_GDP.st$VALUE, start = c(CA_GDP_year.st, CA_GDP_month.st), freq = 12)
#plot(CA_GDP.ts)
#autoplot(CA_GDP.ts)

#get the data form statistic Canada
unemployment_rate_raw <- "v2062815" #Unemployment rate (Percentage); Both sexes; 15 years and over; Estimate; SA, Monthly.
unemployment.st <- get_cansim_vector(unemployment_rate_raw, start_time = "2008/01/01", end_time = Evil.end)
unemployment_rate_year.st <- year(unemployment.st$REF_DATE[1])
unemployment_rate_month.st <- month(unemployment.st$REF_DATE[1])
#transfer data to the time series time
c(unemployment_rate_year.st, unemployment_rate_month.st)
unemployment_rate.ts<- ts(unemployment.st$VALUE, start = c(unemployment_rate_year.st, unemployment_rate_month.st), freq = 12)
#now its time series data!
#plot(unemployment_rate.ts)


#we are assuming we are in no late 2023/1/17 
#Consumer Price Index (CPI)
core_inflation_raw <- "v41690914" #Consumer Price Index, monthly, seasonally adjusted, monthly (2002=100) 
core_inflation.st <- get_cansim_vector(core_inflation_raw, start_time = Evil.begain, end_time = Evil.end)
core_inflation_year.st <- year(core_inflation.st$REF_DATE[1])
core_inflation_month.st <- month(core_inflation.st$REF_DATE[1])
#transfer data to the time series time
c(core_inflation_year.st, core_inflation_month.st)
core_inflation.ts<- ts(core_inflation.st$VALUE, start = c(core_inflation_year.st, core_inflation_month.st), freq = 12)
#now its time series data!
#plot(core_inflation.ts)

#The bank rate 
Bank_rate_raw <- "v122530" #Financial market statistics, last Wednesday unless otherwise stated, Bank of Canada, monthly 
Bank_rate.st <- get_cansim_vector(Bank_rate_raw, start_time = "2008/01/01", end_time = "2022/12/31")
Bank_rate_year.st <- year(Bank_rate.st$REF_DATE[1])
Bank_rate_month.st <- month(Bank_rate.st$REF_DATE[1])

#transfer data to the time series time
c(Bank_rate_year.st, Bank_rate_month.st)
Bank_rate.ts<- ts(Bank_rate.st$VALUE, start = c(Bank_rate_year.st, Bank_rate_month.st), freq = 12)
#now its time series data!
#plot(Bank_rate.ts)

Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2022, 12), frequency = 12)


####################
#5.2 Variable Transformation########################################
#5.2.1 stationary CA_GDP.ts 
stationary_CA_GDP.ts <- 100 *diff(log(CA_GDP.ts), lag = 12)
#5.2.2 leacve unemployment rate as it is  unemployment_rate.ts
#5.2.3 stationary core_inflation.ts
stationary_core_inflation.ts <- 100 * diff(log(core_inflation.ts), lag = 12)
#5.2.4 leave bank rate as it is
#5.2.5 stationary the housing sell 
stationary_Can_month_housing_sell.ts <- 100 * diff(log(Can_month_housing_sell.ts), lag = 12)

#5.2.5 put all data together
all_data <- cbind(stationary_CA_GDP.ts, unemployment_rate.ts, stationary_core_inflation.ts, stationary_Can_month_housing_sell.ts, Bank_rate.ts)
autoplot(all_data)
#The adjustment here

############################################################
VARselect(all_data, lag.max = 10, type="const")
#lag = 4

model_evil <- VAR(all_data, p= 4)


# 5.3 Granger Causality Tests
causality(model_evil, cause = "stationary_CA_GDP.ts")$Granger
causality(model_evil, cause = 'stationary_core_inflation.ts')$Granger
causality(model_evil, cause = 'stationary_Can_month_housing_sell.ts')$Granger
causality(model_evil, cause = 'unemployment_rate.ts')$Granger
causality(model_evil, cause = 'Bank_rate.ts')$Granger

#5.4 the root test
roots(model_evil)
# the root test show that the var is stable.

The_Var_model <- forecast(model_evil, h = 5)

# Extract the forecast for the 'stationary_Can_month_housing_sell.ts' variable
print(The_Var_model$forecast$stationary_Can_month_housing_sell.ts)

plot(stationary_Can_month_housing_sell.ts)


# P


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






































