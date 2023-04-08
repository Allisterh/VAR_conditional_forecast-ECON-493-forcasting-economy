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


#1 The bacis data processing
########################import the data
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

#transfore the data in to ts
Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)

##
# end of training set
n.end <- 2019

# The number of observatin.
n_test <- 12 * (2023 - 2019)

n_models <- 5  #the number of model

pred <- matrix(rep(NA, n_test * (n_models + 1)), nrow =n_test, ncol = (n_models + 1)))

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

#########################################################################################################

#The end of training set
n.end <- 2019
n.test <- (2023 - 2019) * 12
n.model <- 5

#The test set 2020 to 2023
#Set hte matrix for the storeage


pred <- matrix(rep(NA, n_test * (n_models + 1), nrow=n_test, ncol=(n_models + 1)))



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






























