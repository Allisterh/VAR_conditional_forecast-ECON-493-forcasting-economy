## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(fpp2)
library(gridExtra)

# 
options(digits = 4)

# set transparent background 
theme_set(theme_grey())
theme_update(
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

# set the seed
set.seed(1234)



## ---- echo=FALSE, fig.height=3.5-----------------------------------------
data <- read.csv("data/googletrends_applepie_ca.csv", header = FALSE)
canpie <- ts(data$V2, start = c(2013,33), freq = 52.18)
autoplot(canpie) + ylab(" ") + 
  ggtitle("Google Trends: 'Apple Pie Recipe' in Canada")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(a10) + ylab("$ million") + xlab("Year") +
  ggtitle("Antidiabetic drug sales")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
beer <- window(ausbeer, start = 1992)
autoplot(beer) +  xlab("Year") + ylab("megalitres") +
  ggtitle("Australian quarterly beer production")


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# season plot
ggseasonplot(beer, year.labels = TRUE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# subseries plot
ggsubseriesplot(beer)


## ----elecequip-stl, echo=F, fig.height=4.4-------------------------------
fit <- stl(elecequip, s.window = 7)
autoplot(fit) + xlab("Year")


## ----elecequip3, echo=F, fig.height=3.5----------------------------------
# subseries plot
ggsubseriesplot(seasonal(fit))


## ----elecequip-trend, echo=F, fig.height=4.4-----------------------------
autoplot(elecequip, series = "Data") +
  autolayer(trendcycle(fit), series = "Trend-cycle") +
  theme(legend.position = "bottom")


## ----elecequip-sa, echo=FALSE, fig.height=4.4----------------------------
autoplot(elecequip, series = "Data") +
  autolayer(seasadj(fit), series = "Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(
    values = c("gray","blue"),
    breaks = c("Data","Seasonally Adjusted")
  ) +
  theme(legend.position = "bottom")


## ---- echo=TRUE----------------------------------------------------------
# retail data
autoplot(euretail) + xlab("Year") + ylab("Retail index") + 
  ggtitle("European quarterly retail trade")


## ---- echo=TRUE----------------------------------------------------------
# first diff
ggtsdisplay(diff(euretail, lag = 1))


## ---- echo=TRUE----------------------------------------------------------
# seasonal diff
ggtsdisplay(diff(euretail, lag = 4))


## ---- echo=TRUE----------------------------------------------------------
fit1 <- Arima(euretail, order=c(1,1,0), seasonal=c(2,0,0))
checkresiduals(fit1, lag=24, test=FALSE)


## ---- echo=TRUE----------------------------------------------------------
# residuals
checkresiduals(fit1, lag=24, plot=FALSE)


## ---- echo=FALSE---------------------------------------------------------
aicc <- c(
  Arima(euretail, order=c(1,1,0), seasonal=c(2,0,0))$aicc,
  Arima(euretail, order=c(0,1,1), seasonal=c(2,0,0))$aicc,
  Arima(euretail, order=c(1,1,1), seasonal=c(2,0,0))$aicc,
  Arima(euretail, order=c(2,1,0), seasonal=c(2,0,0))$aicc
  )


## ---- echo=FALSE---------------------------------------------------------
aicc <- c(
  Arima(euretail, order=c(1,1,0), seasonal=c(2,0,0))$aicc,
  Arima(euretail, order=c(1,1,0), seasonal=c(3,0,0))$aicc,
  Arima(euretail, order=c(1,1,0), seasonal=c(1,0,0))$aicc,
  Arima(euretail, order=c(1,1,0), seasonal=c(1,0,1))$aicc
  )


## ------------------------------------------------------------------------
# fit model
(fit2 <- Arima(euretail, order=c(1,1,0), seasonal=c(1,0,1)))


## ---- echo=TRUE----------------------------------------------------------
# residuals
checkresiduals(fit2, lag=24, test=FALSE)


## ---- echo=TRUE----------------------------------------------------------
# residuals
checkresiduals(fit2, lag=24, plot=FALSE)


## ---- echo=TRUE----------------------------------------------------------
# residuals
autoplot(forecast(fit2))


## ---- echo=TRUE----------------------------------------------------------
# data and set up
n.end <- 2003.75 # 2003Q4
h.val <- 1
# loop
pred <- matrix(rep(NA,96),32,3)
for(i in 1:32){
  tmp0 <- 1996
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(euretail,tmp0,tmp1)
  pred[i,1] <- window(euretail,tmp1+h.val*.25,tmp1+h.val*.25) # actual 
  # estimate models
  fit1 <- Arima(tmp, order=c(1,1,0), seasonal=c(1,0,1))
  fit2 <- Arima(tmp, order=c(0,1,3), seasonal=c(0,1,1))
  # compute forecasts (h=1)
  pred[i,2] <- forecast(fit1, h=h.val)$mean[h.val]
  pred[i,3] <- forecast(fit2, h=h.val)$mean[h.val]
}


## ---- echo=TRUE----------------------------------------------------------
# compute rmse (h=1)
retailfit1 <- pred[,1] - pred[,2]
retailfit2 <- pred[,1] - pred[,3]
rmse1 <- sqrt(mean(retailfit1^2, na.rm=TRUE))
rmse2 <- sqrt(mean(retailfit2^2, na.rm=TRUE))
# display rmse
cbind(rmse1,rmse2)


## ---- echo=FALSE, fig.height=4.4-----------------------------------------
fcst.all <- 
  ts(
    pred, 
    start = 2004, 
    frequency = 4, 
    names = c("Actual","Model 1","Model 2")
  )
autoplot(euretail) + 
  autolayer(fcst.all) +
  ggtitle("Forecasts for European quarterly retail trade (h=1)") +
  xlab("Year") + ylab("Retail index") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")


## ---- echo=TRUE----------------------------------------------------------
# data and set up
n.end <- 2003.75 # 2003Q4
h.val <- 4
# loop
pred <- matrix(rep(NA,87),29,3)
for(i in 1:29){
  tmp0 <- 1996
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(euretail,tmp0,tmp1)
  pred[i,1] <- window(euretail,tmp1+h.val*.25,tmp1+h.val*.25) # actual 
  # estimate models
  fit1 <- Arima(tmp, order=c(1,1,0), seasonal=c(1,0,1))
  fit2 <- Arima(tmp, order=c(0,1,3), seasonal=c(0,1,1))
  # compute forecasts (h=4)
  pred[i,2] <- forecast(fit1, h=h.val)$mean[h.val]
  pred[i,3] <- forecast(fit2, h=h.val)$mean[h.val]
}


## ---- echo=TRUE----------------------------------------------------------
# compute rmse (h=4)
retailfit1 <- pred[,1] - pred[,2]
retailfit2 <- pred[,1] - pred[,3]
rmse1 <- sqrt(mean(retailfit1^2, na.rm=TRUE))
rmse2 <- sqrt(mean(retailfit2^2, na.rm=TRUE))
# display rmse
cbind(rmse1,rmse2)


## ---- echo=FALSE, fig.height=4.4-----------------------------------------
fcst.all <- 
  ts(
    pred, 
    start = 2004.75, 
    frequency = 4, 
    names = c("Actual","Model 1","Model 2")
  )
autoplot(euretail) + 
  autolayer(fcst.all) +
  ggtitle("Forecasts for European quarterly retail trade (h=4)") +
  xlab("Year") + ylab("Retail index") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# fit model
(fit <- auto.arima(euretail))


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit, test=FALSE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit, plot=FALSE)

