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
library("readxl")


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

allowdrift = TRUE
stepwise = TRUE

#now, its time to fit the model.



#becase
Model_1<- Arima(Can_month_housing_sell.ts, order = c(0, 1, 1))
evil_forecast <- forecast(Model_1, h=5)
plot(evil_forecast)

(fit2 <- auto.arima(Can_month_housing_sell.ts))


#Housing_sell_per_covid <- ts(News_release_chart_data_mar_2023$Canada, start = c(2007, 1), end = c(2019, 6), frequency = 12)
#autoplot(Housing_sell_per_covid)
#model_pre_covid <- auto.arima(Housing_sell_per_covid)
#model_pre_covid_forecast <- forecast(model_pre_covid, h = 24)
#plot(model_pre_covid_forecast)

Housing_sell_after_covid_start_2018 <- ts(News_release_chart_data_mar_2023$Canada, start = c(2018, 1), end = c(2023, 3), frequency = 12)

#checkresiduals(Housing_sell_after_covid_start_2018)
#The data is not stationary.


diff_Housing_sell_after_covid_start_2018 <- diff(Housing_sell_after_covid_start_2018, lag = 1)
plot(diff_Housing_sell_after_covid_start_2018)
#checkresiduals(diff_Housing_sell_after_covid_start_2018)

seasonal_dff_diff_Housing_sell_after_covid_start_2018 <- diff(diff_Housing_sell_after_covid_start_2018, lag = 12)
checkresiduals(seasonal_dff_diff_Housing_sell_after_covid_start_2018)

model_seaonal_diff <-auto.arima(diff_Housing_sell_after_covid_start_2018, d = 1, D = 1)
model_Housing_sell_after_covid_start_2018 <- Arima(Housing_sell_after_covid_start_2010, order = c(2, 0, 0), seasonal = list(order = c(0, 0, 1), period = 12))



#adf.test(Housing_sell_after_covid_start_2018)
#adf test show that the data may not be stationary. 
#adf.test(diff(Housing_sell_after_covid_start_2018))
#The data is staionary.

#model 1 with d =1
#auto.arima(Housing_sell_after_covid_start_2018, d = 1)
#model_Housing_sell_after_covid_start_2018_d_1 <- Arima(Housing_sell_after_covid_start_2010, order = c(1, 1, 0), seasonal = list(order = c(0, 0, 1), period = 12))

#model w
auto.arima(Housing_sell_after_covid_start_2018, d = 1)
model_Housing_sell_after_covid_start_2018 <- Arima(Housing_sell_after_covid_start_2010, order = c(2, 0, 0), seasonal = list(order = c(0, 0, 1), period = 12))

#accuracy(data = Housing_sell_after_covid_start_2018, model_Housing_sell_after_covid_start_2018_d_1)
#accuracy(data = Housing_sell_after_covid_start_2018, model_Housing_sell_after_covid_start_2018)


# the one with autoarima better, so I just remove rest of it.

model_Housing_sell_after_covid_start_2018<- forecast(model_Housing_sell_after_covid_start_2010, h = 5)
autoplot(model_Housing_sell_after_covid_start_2018)

summary(model_Housing_sell_after_covid_start_2018)



checkresiduals(model_Housing_sell_after_covid_start_2018)
Pacf(model_Housing_sell_after_covid_start_2018$residuals)
#still really strong autocorrlation,



