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
#ARIMA(2,1,0) 

autoplot(forecast(The_before_during_after_Covid_model, h = 5))

checkresiduals(The_before_during_after_Covid_model)
(The_before_during_after_Covid_model)

#ARIMA(2,1,0)
print(The_before_during_after_Covid_model)


#4.constructed model 2: what if we only including the data from 2022 alone? 
#######
The_2022_along.data<-window(Can_month_housing_sell.ts, start= c(2022,1), end= c(2023,2))
The_2022_along.data_model <- auto.arima(The_2022_along.data, approximation = FALSE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
print(The_2022_along.data_model)
autoplot(forecast(The_2022_along.data_model, h = 2))


#############################################################################################################################



#Part2: play around the housing sell data with other data
#############################################################################################################################

#2.2 The data base import 
everyhingCA_stationary_raw <- read_csv("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/LCDMA_February_2023/balanced_can_md.csv")
#View(everyhingCA.raw)

everyhingCA_raw<- read_csv("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/LCDMA_February_2023/CAN_MD.csv") 
#View(everyhingCA_raw)
######## that is too much data there....


############ 2.2 Data process


#2.2.1 Housing starts (units)
#plot(everyhingCA_raw$build_Comm_CAN_new) #the data look good
#summary(everyhingCA_raw$build_Comm_CAN_new)
hstart_CAN_new_raw <- everyhingCA_raw$hstart_CAN_new
hstart_CAN_new.omit <- na.omit(hstart_CAN_new_raw)
hstart_CAN_new.ts <- ts(hstart_CAN_new.omit, start = c(1990, 1), end = c(2023, 1), frequency = 12)
#autoplot(hstart_CAN_new.ts)
#hstart_CAN_new_model <- auto.arima(log(hstart_CAN_new.ts), approximation = FALSE, parallel = T,stepwise = FALSE, max.p = 5,max.q = 5, max.d = 5, max.Q = 5, max.P = 5, max.D = 5)
#print(hstart_CAN_new_model)
#The data look good.


plot(hstart_CAN_new.ts *100)
plot(Can_month_housing_sell.ts)
#2.2.2






#v111955443 New housing price indexes house only, frequence = 12
#v121294115 Seasonally adjusted, Building permits, by type of structure and type of work, current,frequence = 12
#v122550 bank rate




#2.1 The new
#housing sell with housing permit.
#########
unemployment_rate_raw <- "v111955454"
unemployment.st <- get_cansim_vector(unemployment_rate_raw, start_time = "2000-01-01")
unemployment_rate_year.st <- year(unemployment.st$REF_DATE[1])
unemployment_rate_month.st <- month(unemployment.st$REF_DATE[1])

#transfer data to the time series time
c(unemployment_rate_year.st, unemployment_rate_month.st)
unemployment_rate.ts<- ts(unemployment.st$VALUE, start = c(unemployment_rate_year.st, unemployment_rate_month.st), freq = 12)
#now its time series data!
autoplot(unemployment_rate.ts)


unemployment_rate_raw v730413




########### The following code just for fun
#autoplot(unemployment_rate.ts)
#####
#unemployment_rate_model <- auto.arima(unemployment_rate.ts, approximation = FALSE, parallel = TRUE, stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5)
#print(unemployment_rate_model)
#checkresiduals(unemployment_rate_model)
#unemployment_rate_model_2 <- auto.arima(Can_month_housing_sell.ts, xreg=unemployment_rate.ts)
#checkresiduals(unemployment_rate_model_2)
#forecast_fit1 <- forecast(unemployment_rate_model_2, xreg=unemployment_rate.ts, h=2)
#autoplot(forecast_fit1)

############################


