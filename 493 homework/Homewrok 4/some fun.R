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

############################################################

library(readxl)

sing_sell_supre_raw <- read_excel("The research project/Tie Ma - UofAB - unadjusted residential monthly sales - provincial - Jan 1980 to Feb 2023.xlsx")

plot(sing_sell_supre_raw$Canada, type = "l")


Canadian_housing_resale.ts<- ts(sing_sell_supre_raw$Canada, end = c(2023/02), start = c(1980/01), frequency = 12)


autoplot(diff(log(Alberta_unit_sell)))


checkresiduals(Alberta_unit_sell)

adf.test(Alberta_unit_sell)


Alberta_unit_sell_model<- auto.arima(Alberta_unit_sell
                                     ,approximation = FALSE, parallel = T, 
                                     stepwise = FALSE, max.P = 10, 
                                     max.Q = 10, max.D = 10,
                                     max.d = 10, max.p = 10, max.q = 10)
print(Alberta_unit_sell_model)

autoplot(forecast(Alberta_unit_sell_model, h = 24))               

 

