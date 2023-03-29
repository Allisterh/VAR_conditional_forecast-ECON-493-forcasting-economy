#495. 
#Tie.Ma. 
#1537905. 

#Step one Lode the all package that necessary. 
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

#import the data

library(readxl)
News_release_chart_data_mar_2023 <- read_excel("/Users/tie/Documents/GitHub/ECON-493-forcasting-economy/The research project/News_release_chart_data_mar_2023.xlsx", 
                                               sheet = "Chart A", col_types = c("date", 
                                                                                "numeric", "numeric", "skip", "skip"))

Can_month_housing_sell.ts <- ts(News_release_chart_data_mar_2023$Canada, start = c(2007, 1), end = c(2023, 2), frequency = 12)

#plot the graphy to check it.
#autoplot(Can_month_housing_sell.ts)
#graphy look good.

#check the residuals.
#Are data is stationary? 
checkresiduals(Can_month_housing_sell.ts)
Acf(Can_month_housing_sell.ts)
Pacf(Can_month_housing_sell.ts)

#The data is not stationary. 
#stationary data.
Can_month_housing_sell_df.ts <- diff(Can_month_housing_sell.ts, lag = 1)
summary
checkresiduals(Can_month_housing_sell_df.ts)
Acf(Can_month_housing_sell_df.ts)
Pacf(Can_month_housing_sell_df.ts)





