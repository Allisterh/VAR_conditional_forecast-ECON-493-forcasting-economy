#495. 
#Tie.Ma. 
#1537905. 

#Step one Lode the all package that necessary. 
######################
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

#Index
  #the data process
  #play with data alone
    #all data ARIMA model
    #After 2008 ARIMA model
    #before and after covid ARIMA model
    #After covid model.






#1.1 The data processing
########################import the data
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)
autoplot(Can_month_housing_sell.ts)

#plot the graphy to check it.
#autoplot(Can_month_housing_sell.ts)
#graphy look good.
##########################

#Are data is stationary? 
checkresiduals(Can_month_housing_sell.ts)
Acf(Can_month_housing_sell.ts)
Pacf(Can_month_housing_sell.ts)
adf.test(Can_month_housing_sell.ts)
kpss.test(Can_month_housing_sell.ts)


#all test suggest this time series is not stationary
###########################

#stationary data.
Can_month_housing_sell_df.ts <- diff(Can_month_housing_sell.ts, lag = 1)
autoplot(Can_month_housing_sell_df.ts)

checkresiduals(Can_month_housing_sell_df.ts)
Acf(Can_month_housing_sell_df.ts)
Pacf(Can_month_housing_sell_df.ts)
#The graph look stationary enough.

#Doing the union root test.
adf.test(Can_month_housing_sell_df.ts)
kpss.test(Can_month_housing_sell_df.ts)
# The union root test tell the data is stationary.



#almost statioanry. 
#########################



#2.0 all_data_model_one
########################

#trace = TRUE
  #输出诊断信息到控制台，以便更好地理解模型选择过程。
#approximation = FALSE
  #不使用近似方法加速模型选择过程。这样可以确保算法能够尝试所有可能的模型


#now, its time to fit the model.
#because the name maybe too long so I just use model 1 or 2 to t

all_data_model_1 <- auto.arima(Can_month_housing_sell.ts, approximation = FALSE, parallel = TRUE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
#ARIMA(2,1,0)
print(all_data_model_1)

#check the model
checkresiduals(all_data_model_1)
Pacf(all_data_model_1$residuals)

#it look goooood

########### forecasting time
all_data_forecast <- forecast(all_data_model_1, h = 5)
autoplot(all_data_forecast)



#3 after_2008_forecasting.
#########################

#constructed model 2: what if we do not including the data from before the 2008 the economic crisis?
after_2008_forecasting_data <- window(Can_month_housing_sell.ts, start= c(2009,7), end= c(2023,2))

#plot the graphy
autoplot(after_2008_forecasting_data)

#the data look coool, now it is the time to play with data.
after_2008_forecasting_model <- auto.arima(after_2008_forecasting_data, approximation = FALSE, parallel = TRUE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
#ARIMA(2,1,0)
print(after_2008_forecasting_model)

#check the model
checkresiduals(after_2008_forecasting_model)
Pacf(after_2008_forecasting_model$residuals)


########### forecasting time
after_2008_forecasting_model_forecast <- forecast(after_2008_forecasting_model, h = 5)
autoplot(after_2008_forecasting_model_forecast)


######### compare those two model.

########### the graphic
# Plot the time series data
plot(Can_month_housing_sell.ts)

# Add the forecast from the after_2008_forecasting_model
lines(forecast(after_2008_forecasting_model, h = 5)$mean, col = "blue")

# Add the forecast from the all_data_forecast model
lines(forecast(all_data_model_1, h = 5)$mean, col = "red")

# Add a legend
legend("topright", legend = c("Original data", "After 2008 forecast", "All data forecast"), col = c("black", "blue", "red"), lty = 1)

#############################



#constructed model 2: what if we only including the data from 2016 - 2023 alone? 
#The_before_during_after_Covid_model.
#######################

####################### process the data. 
The_before_during_after_Covid_model.data<-window(Can_month_housing_sell.ts, start= c(2017,1), end= c(2023,2))

#plot the graphy
#autoplot(The_before_during_after_Covid_model.data)
#checkresiduals(The_before_during_after_Covid_model.data)
#diff_The_before_during_after_Covid_model.data <- diff(The_before_during_after_Covid_model.data, lag = 1)
#adf.test(diff_The_before_during_after_Covid_model.data)
#yep, the data is staionary. 


#the data look coool, now it is the time to play with data.
The_before_during_after_Covid_model <- auto.arima(The_before_during_after_Covid_model.data, approximation = FALSE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
print(The_before_during_after_Covid_model)
The_before_during_after_Covid_model_forecast <- forecast(The_before_during_after_Covid_model, h = 5)
#ARIMA(2,1,0) 

autoplot(The_before_during_after_Covid_model_forecast)

checkresiduals(The_before_during_after_Covid_model)
(The_before_during_after_Covid_model)

#ARIMA(2,1,0)
print(The_before_during_after_Covid_model)


#4.constructed model 2: what if we only including the data from 2022 alone? 
#######
The_2022_along.data<-window(Can_month_housing_sell.ts, start= c(2022,1), end= c(2023,2))
The_2022_along.data_model <- auto.arima(The_2022_along.data, approximation = FALSE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
print(The_2022_along.data_model)
The_2022_along.data_model_forecast(The_2022_along.data_model, h = 5)

##################

# Load necessary libraries and data
library(forecast)

# Load the four ARIMA models
The_2022_along.data_model <- arima(...)
The_before_during_after_Covid_model <- arima(...)
after_2008_forecasting_model <- arima(...)
all_data_model_1 <- arima(...)

# Create a matrix to store the forecasts
forecasts <- matrix(rep(NA,24),6,4)

# Generate forecasts for the next 6 periods using each model
forecasts[,1] <- forecast(The_2022_along.data_model,h=6)$mean
forecasts[,2] <- forecast(The_before_during_after_Covid_model,h=6)$mean
forecasts[,3] <- forecast(after_2008_forecasting_model,h=6)$mean
forecasts[,4] <- forecast(all_data_model_1,h=6)$mean

# Plot the forecasts in one graph
plot.ts(forecasts, main = "Forecast Graph for 4 ARIMA Models", xlab = "Period", ylab = "Forecasted Values", col = 1:4)



#############################################################################################################################



#Part2: play around the housing sell data with other data
#############################################################################################################################

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
housing_sell_plus_policy_rate_model<- auto.arima(Can_month_housing_sell.ts, d =1, xreg = CA_bank_rate_diff.ts, approximation = FALSE, parallel = T, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
print(housing_sell_plus_policy_rate_model)
checkresiduals(housing_sell_plus_policy_rate_model)


evil_forcast <- forecast(housing_sell_plus_policy_rate_model, h = 10, x = c(0.25,0.25,5,0.25,0.25,0.25,0.25,0.25))
plot(evil_forcast)

##################

################# The relationship between housing sell and GPD?
CA_GDP_raw <- "v65201483"
CA_GDP.st <- get_cansim_vector(CA_GDP_raw, start_time = "2006-01-01", end_time = "2023-01-01")
CA_GDP_year.st <- year(CA_GDP.st$REF_DATE[1])
CA_GDP_month.st <- month(CA_GDP.st$REF_DATE[1])

#transfer data to the time series time
c(CA_GDP_year.st, CA_GDP_month.st)
CA_GDP.ts <-ts(CA_GDP.st$VALUE, start = c(CA_GDP_year.st, CA_GDP_month.st), freq = 12)

plot(CA_GDP.ts)
CA_GDP_log_diff.ts <- diff(log(CA_GDP.ts))
plot(CA_GDP_log_diff.ts)
adf.test(CA_GDP_log_diff.ts)

# create a new time series object using a subset of the data
Can_month_housing_sell_GDP_model.ts <- window(Can_housing_sell_data.raw$Canada, start = c(2007,1), end = c(2022,12))

# display the resulting time series
Can_month_housing_sell_GDP_model.ts


housing_sell_plus_GDP_model<- auto.arima(Can_month_housing_sell_GDP_model.ts, d =1, xreg = CA_GDP_log.ts, approximation = FALSE, parallel = T, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
print(housing_sell_plus_GDP_model)



