#495. 
#Tie.Ma. 
#1537905. 

#Step one Lode the all package that necessary or not? 
#yes, I just copy it around, so I did not need to check around.

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
  library("lubridate")
##################

#Index
  #the data process
  #play with data alone
    #all data ARIMA model
    #After 2008 ARIMA model
    #before and after covid ARIMA model
    #After covid model.



#1.1 The bacis data processing
########################import the data
Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

Can_month_housing_sell.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)
#transfor the data to the ts

autoplot(Can_month_housing_sell.ts, 
         xlab = "time", ylab = "housing sell", main = "Canadian housing sell", 
         geom = c("line"), label = T, truncate.gaps = FALSE,
         Label.color = "#90aac0",  colour = "#003b6f" ) + 
  theme(plot.background = element_rect(fill = "#C0CEDB"),
        panel.background = element_rect(fill = "#E0E7ED"),
        plot.caption = element_text(hjust = 0, vjust = 0, margin = margin(t = 5, unit = "pt")))

#plot the graphy to check it.
#autoplot(Can_month_housing_sell.ts)
#graphy look good.

##########################

#Are data is stationary? 
adf.test(Can_month_housing_sell.ts)
kpss.test(Can_month_housing_sell.ts)
#all test suggest this time series is not stationary

###########################

#stationary data.
Can_month_housing_sell_df.ts <- diff(Can_month_housing_sell.ts, lag = 1)
autoplot(Can_month_housing_sell_df.ts)

adf.test(Can_month_housing_sell_df.ts)
kpss.test(Can_month_housing_sell_df.ts)
# The union root test tell the data is stationary.
#almost statioanry. 
#########################


#2.0 all_data_model_one
#just play around the data for fun.
########################

#trace = TRUE
  #输出诊断信息到控制台，以便更好地理解模型选择过程。
#approximation = FALSE
  #不使用近似方法加速模型选择过程。这样可以确保算法能够尝试所有可能的模型


#now, its time to fit the model.
#because the name maybe too long so I just use model 1 or 2 to t

### This code force auto,arima to search 15625 combinations of ARIMA
    #from ARIMA(1,1,1)(1,1,1) go to ARIMA (5,5,5)(5,5,5)
    #it will consume a huge amount of computing point, it took me 36s

all_data_model_1 <- auto.arima(Can_month_housing_sell.ts, 
                                approximation = FALSE, 
                                parallel = TRUE, 
                                stepwise = FALSE,
                                d = 1,
                                max.Q = 5, max.P = 5, max.D = 5,
                                max.d = 5, max.p = 5, max.q = 5)
print(all_data_model_1)     
#ARIMA(2,1,0) 

### so, what dos the default autoarima will give me and how long it take?
#0.025s and ARIMA(0,1,1)

all_data_model_2 <- auto.arima(Can_month_housing_sell.ts)
print(all_data_model_2)
#ARIMA(0,1,1) random walk with draft.

#######################so, which one is better? 

######compare the AIC?
AIC(all_data_model_2)
#AIC = 3583.642
AIC(all_data_model_1)
#AIC = 3582.467
#while, they both seem good almost idendical.
#and the data seem to be randomly walk...
#####################
#fail try to plot two graphy and comapre

#fc1 <- forecast(all_data_model_2, h = 10)
#fc2 <- forecast(all_data_model_1, h=10)
#autoplot(Can_month_housing_sell.ts) +
 # autolayer(fc2, series="Stochastic trend") +
  #autolayer(fc1, series="Deterministic trend") +
  #ggtitle("Forecasts from trend models") +
  #xlab("Year") + ylab("Visitors to Australia (millions)") +
#  guides(colour=guide_legend(title="Forecast")) +
 # theme(legend.position = "bottom")

##########

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



#3. while if we only forcast the data after 2008?
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
                               d = 1,
                               max.Q = 5, max.P = 5, max.D = 5,
                               max.d = 5, max.p = 5, max.q = 5)
#ARIMA(2,1,0)
print(after_2008_forecasting_model_1)

after_2008_forecasting_model_2 <- auto.arima(after_2008_forecasting_data)
print(after_2008_forecasting_model_2)
#ARIMA(0,1,1) still random walk.
#######

######
#which one is better?
AIC(after_2008_forecasting_model_2)
#3041.171
AIC(after_2008_forecasting_model_1)
#3038.807
########


##########

########### forecasting time
after_2008_forecasting_model_forecast <- forecast(after_2008_forecasting_model, h = 5)
autoplot(after_2008_forecasting_model_forecast)

#############

######### compare those two model: the one with full data and the data after 2008? 
########### the graphic
# Plot the time series data
#autoplot(Can_month_housing_sell.ts)
# Add the forecast from the after_2008_forecasting_model
#autoplot(forecast(after_2008_forecasting_model, h = 20)$mean, col = "blue")
# Add the forecast from the all_data_forecast model
#lines(forecast(all_data_model_1, h = 20)$mean , col = "red")
# Add a legend
#legend("topright", legend = c("Original data", "After 2008 forecast", "All data forecast"), col = c("black", "blue", "red"), lty = 1)
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

The_diff_before_during_after_Covid_model.data <- diff(The_before_during_after_Covid_model.data, lag=1)

The_before_during_after_Covid_model_1 <- auto.arima(The_diff_before_during_after_Covid_model.data, 
                                             approximation = FALSE, 
                                             parallel = T, 
                                             stepwise = FALSE,
                                             max.Q = 5, max.P = 5, max.D = 5,
                                             max.d = 5, max.p = 5, max.q = 5)
print(The_before_during_after_Covid_model_1)
#ARIMA(2,1,0) 

#####
The_before_during_after_Covid_model_2 <- auto.arima(The_before_during_after_Covid_model.data, d=1)
print(The_before_during_after_Covid_model_2)
#ARIMA(0,1,1) the random walk drift.

The_before_during_after_Covid_model_2_forecast <- forecast(The_before_during_after_Covid_model_1, h = 5)
autoplot(The_before_during_after_Covid_model_2)

##### Compare the model.
AIC(The_before_during_after_Covid_model_1)
#1413.121
AIC(The_before_during_after_Covid_model_2)
#1413.473
######


checkresiduals(The_before_during_after_Covid_model)
(The_before_during_after_Covid_model)

#ARIMA(2,1,0)
print(The_before_during_after_Covid_model)


#4.constructed model 2: what if we only including the data from 2022 alone? 
#######
The_2022_along.data<-window(Can_month_housing_sell.ts, start= c(2022,1), end= c(2023,2))

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


##################

# Create a matrix to store the forecasts
forecasts <- matrix(rep(NA,24),6,4)

# Generate forecasts for the next 6 periods using each model
forecasts[,1] <- forecast(The_2022_along_model_1,h=6)$mean
forecasts[,2] <- forecast(The_before_during_after_Covid_model_1,h=6)$mean
forecasts[,3] <- forecast(after_2008_forecasting_model_1,h=6)$mean
forecasts[,4] <- forecast(all_data_model_1,h=6)$mean

# Plot the forecasts in one graph
plot.ts(forecasts, main = "Forecast Graph for 4 ARIMA Models", xlab = "Period", ylab = "Forecasted Values", col = 1:4)

#some conclusion, my data along are close to the random walk........
#help?



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
hourly_average_wag.st <- get_cansim_vector(hourly_average_wage_raw, start_time = "2005-12-01", end_time = "2022-12-01")
hourly_average_wage_year.st <- year(hourly_average_wag.st$REF_DATE[1])
hourly_average_wage_month.st <- month(hourly_average_wag.st$REF_DATE[1])
#transfer data to the time series time
c(hourly_average_wage_year.st, hourly_average_wage_month.st)
hourly_average_wage.ts <-ts(hourly_average_wag.st$VALUE, start = c(hourly_average_wage_year.st, hourly_average_wage_month.st), freq = 12)
plot(hourly_average_wage.ts)

log_hourly_average_wage.ts <- diff(diff(hourly_average_wage.ts, lag = 12))

adf.test(log_hourly_average_wage.ts)

autoplot(log_hourly_average_wage.ts *100)

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

housing_sell_plus_hourly_wage_model<- auto.arima(Can_month_housing_sell_GDP_model.ts, xreg=(log_hourly_average_wage.ts*100), approximation = FALSE, parallel = T, stepwise = FALSE, 
                                                 max.d = 5, max.p = 5, max.q = 5)
#Regression with ARIMA(1,1,3) errors 
print(housing_sell_plus_hourly_wage_model)

x <- 36
# initialize an empty vector to store the values
values <- numeric(length = x)
for (i in 1:x) {
  values[i] <- 
}
The_wage_incerase_rata <- print(values)

housing_sell_plus_hourly_wage_model_forecast <- forecast( housing_sell_plus_hourly_wage_model, h = 36, xreg = The_wage_incerase_rata)
autoplot(housing_sell_plus_hourly_wage_model_forecast)






