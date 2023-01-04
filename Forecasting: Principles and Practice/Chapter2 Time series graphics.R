install.packages("fpp2")
library(fpp2)

autoplot(melsyd[,"Economy.Class"]) + 
  ggtitle("Economy class passengers: Melbourne-Sydney") + 
  xlab("Year") +
  ylab("Thousands")

#the seasonal data
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

#The seasonal graphy
#show the different change in the different graphy
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

#季节性地块的一个有用变体使用极坐标。设置polar=TRUE使时间序列轴呈圆形而不是水平，如下所示。
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

#强调季节性模式的替代情节是将每个季节的数据一起收集在单独的迷你时间图中。
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")
#水平线表示每个月平均值。这种形式的情节可以清楚地看到潜在的季节性模式
#并显示了随着时间的推移季节性的变化。它对于识别特定季节内的变化特别有用

#2.6 Scatterplots
#The graphs discussed so far are useful for visualising individual time series. 
#It is also useful to explore relationships between time series.
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

#Correlation
#It is common to compute correlation coefficients to measure the strength of the relationship between two variables. 
#The value of  r always lies between  −1 and 1 with negative values indicatinga negative 
#relationship and positive values indicating a positive relationship

# 2.7 auto correlations
  # autocorrelation measures the linear relationship between lagged values of a time series.

#Trend and seasonality in ACF plots
  # So the ACF of trended time series tend to have positive values that slowly decrease as the lags increase.

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)

#the white noise
#Time series that show no autocorrelation are called white noise.
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)

#the practice
#read the cvs file
tute1 <- read.csv("data/tute1.csv", header=TRUE)
View(tute1)
# transfer it to the time series file
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)
autoplot(mytimeseries, facets=TRUE)
# with the face will put a line?
ggseasonplot(mytimeseries)
#without the facets=ture, it put all the graphy together
autoplot(mytimeseries)


#read the cvs file 
retaildata <- readxl::read_excel("data/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))

autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts)
ggseasonplot(a10, polar=TRUE)
