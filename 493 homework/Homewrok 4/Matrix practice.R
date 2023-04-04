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
##################

#################### let's compare ARIMA (2,1,0) with ARIMA(0,1,1) to see which one is btter .--



#### set up the test enviroment 

Can_housing_sell_data.raw <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", sheet = "Chart A", col_types = c("date",  "numeric", "numeric", "skip", "skip"))
#yes,they do got other data, which is boring to be honest. 

Can_month_housing_sell_without_covid_shock.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2017, 12), frequency = 12)
#transfor the data to the ts
plot(Can_month_housing_sell_without_covid_shock.ts)

###### set up model

diff_Can_month_housing_sell.ts <- diff(Can_month_housing_sell.ts)

#test_model_one<- auto.arima(Can_month_housing_sell.ts, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)
#print(test_model_one)
the_model_without_covid_shock <- auto.arima(Can_month_housing_sell.ts)
#ARIMA(2,0,2)(0,0,1)[12] with zero mean 
#note: both give the same outcome.

######
Can_month_housing_sell_with_covid_shock.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2019, 12), frequency = 12)
test_set.ts <- ts(Can_housing_sell_data.raw$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)


autoplot(Can_month_housing_sell_with_covid_shock.ts)

The_model_with_covid_shock_one <- auto.arima(Can_month_housing_sell_with_covid_shock.ts, approximation = FALSE, parallel = T,stepwise = FALSE, max.Q = 5, max.P = 5, max.D = 5,max.d = 5, max.p = 5, max.q = 5)
The_model_with_covid_shock_two <- auto.arima(Can_month_housing_sell_with_covid_shock.ts)
###########

#three different model, which one perform better?
the_model_without_covid_shock
The_model_with_covid_shock_one 
The_model_with_covid_shock_two
###########

#let's compare with the 3 model

AIC(the_model_without_covid_shock)
AIC(The_model_with_covid_shock_one)
AIC(The_model_with_covid_shock_two)


# Fit the three ARIMA models
the_model_without_covid_shock <- Arima(Can_month_housing_sell_without_covid_shock.ts, order = c(2,1,2), seasonal = list(order = c(0,0,1), period = 12))
The_model_with_covid_shock_one <- Arima(Can_month_housing_sell_without_covid_shock.ts, order = c(2,1,0))
The_model_with_covid_shock_two <- Arima(Can_month_housing_sell_without_covid_shock.ts, order = c(0,1,1))

# Generate forecasts for the three models
forecast_1 <- forecast(the_model_without_covid_shock, h = length(test_set.ts))
forecast_2 <- forecast(The_model_with_covid_shock_one, h = length(test_set.ts))
forecast_3 <- forecast(The_model_with_covid_shock_two, h = length(test_set.ts))

# Calculate accuracy measures for the three models
acc_1 <- accuracy(forecast_1, test_set.ts)
acc_2 <- accuracy(forecast_2, test_set.ts)
acc_3 <- accuracy(forecast_3, test_set.ts)

# Print out the accuracy measures for the three models
cat("Accuracy measures for the model without Covid shock:\n")
print(acc_1)

cat("Accuracy measures for the model with Covid shock one:\n")
print(acc_2)

cat("Accuracy measures for the model with Covid shock two:\n")
print(acc_3)

#############
#play around with data 2








