#Chapter 3 The forecaster’s toolbox\
#3.1 some simple forecasting method
  # average method
  #meanf(y, h)
    # y contains the time series
    # h is the forecast horizon'

#Native moethod
#Native (y,h)
#rwf(y, h) # Equivalent alternative
#Because a naïve forecast is optimal when data follow a random walk (see Section 8.1),
#these are also called random walk forecasts.

#Seasonal naïve method
#snaive(y, h)]
#A similar method is useful for highly seasonal data. In this case, we set each forecast to be equal 
#to the last observed value from the same season (e.g., the same month of the previous year). 

#Drift method
#A variation on the naïve method is to allow the forecasts to 
#increase or decrease over time, where the amount of change over time 


# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# 3.2 Transfomations and adjustments
#Calendar addjustments
d阿斯顿
