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
libaray("vars")

########################################################################


#how many lags in the var? 
  #the lag lenght for var model may be determined using model selection criteria. 
  #for forecasting with Var model, we perfer to using the BIC.


#VARselect() for selecting the number of lags p using different information criteria
#VAR() for fitting VAR models
#Serial.test() for theresiduals test. 



# read quarterly data
data <- read.csv("493 Class note/Advanced topics-20230405/us_macro_quarterly.csv", header = TRUE)

# get time series
gdpgr <- 400*diff(log(data$GDPC96)) 
#convert from a quarterly frequency to an annual frequency


tsprd <- data$GS10-data$TB3MS
#he difference between the 10-year Treasury bond yield (GS10) 
#the 3-month Treasury bill yield (TB3MS) for each observation in the "data" object.

tsprd <- tsprd[-1]
#set each variable to the lag one

# full sample
vardata0 <- ts(cbind(gdpgr,tsprd), start=c(1957,2), frequency=4)
vardata0 <- window(vardata0, start=c(1984,1), end=c(2012,4))


#plot the data
autoplot(vardata0, facets = TRUE, colour=TRUE) + 
  ylab("") + xlab("") + guides(colour="none")











