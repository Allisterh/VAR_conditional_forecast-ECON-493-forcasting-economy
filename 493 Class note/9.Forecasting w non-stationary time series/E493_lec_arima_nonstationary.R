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



## ---- fig.height=3.5-----------------------------------------------------
# simulate ARIMA(1,1,0)
y1.ar <- arima.sim(list(order = c(1,1,0), ar = 0.5), n = 200)
autoplot(y1.ar)


## ---- fig.height=3.5-----------------------------------------------------
# plot stationary first difference
dy1.ar <- diff(y1.ar)
autoplot(dy1.ar)


## ---- fig.height=3.5-----------------------------------------------------
# fit AR(1) to first difference
(fit1 <- Arima(dy1.ar, order = c(1,0,0)))


## ---- fig.height=3.5-----------------------------------------------------
# plot forecasts
autoplot(forecast(fit1, h = 50), main = "")


## ---- fig.height=3.5-----------------------------------------------------
# fit ARIMA(1,1,0)
(fit2 <- Arima(y1.ar, order = c(1,1,0), include.drift = TRUE))


## ---- fig.height=3.5-----------------------------------------------------
# plot forecasts
autoplot(forecast(fit2, h = 50), main = "")


## ----ee0, fig.height=3.5-------------------------------------------------
# plot electrical equipment index
autoplot(elecequip) + xlab("Year") + ylab("index") +
  ggtitle("New orders index")


## ----ee1, fig.height=3.5-------------------------------------------------
# plot electrical equipment index
eeadj <- seasadj(stl(elecequip, s.window = "periodic"))
autoplot(eeadj) + xlab("Year") + ylab("index") +
  ggtitle("Seasonally adjusted new orders index")


## ----ee2, fig.height=3.5-------------------------------------------------
# plot electrical equipment index, first difference
ggtsdisplay(diff(eeadj))


## ---- fig.height=3.5-----------------------------------------------------
# fit model to first difference
(fit <- Arima(eeadj, order=c(3,1,0), include.drift = TRUE))


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(fit, test = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(fit, plot = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# plot forecasts
autoplot(forecast(fit), main = "")


## ------------------------------------------------------------------------
# read quarterly data
data <- read.csv("data/us_macro_quarterly.csv", header = TRUE)

# get time series
rgdp.lv <- ts(data$GDPC96, start = c(1957,1), freq = 4)
rgdp.gr <- ts(
  400*diff(log(data$GDPC96)), 
  start = c(1957,2), 
  freq = 4
)


## ---- fig.height=3.5-----------------------------------------------------
# plot RGDP
autoplot(rgdp.lv) + xlab("Year") + ylab(" ") 


## ---- fig.height=3.5-----------------------------------------------------
# plot RGDP growth
autoplot(rgdp.gr) + xlab("Year") + ylab(" ")


## ---- fig.height=3.5-----------------------------------------------------
# in-sample (training set): 1970Q1-2000Q4
yt <- window(rgdp.gr, end = c(2000,4))
ggtsdisplay(yt)


## ---- fig.height=3.5-----------------------------------------------------
# AR(1), in-sample
(model10 <- Arima(yt, order = c(1,0,0)))


## ---- fig.height=3.5-----------------------------------------------------
# AR(1), in-sample
checkresiduals(model10, lag = 10, test = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# AR(1), in-sample
checkresiduals(model10, lag = 10, plot = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# MA(1), in-sample
(model01 <- Arima(yt, order = c(0,0,1)))


## ---- fig.height=3.5-----------------------------------------------------
# MA(1), in-sample
checkresiduals(model01, lag = 10, test = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# MA(1), in-sample
checkresiduals(model01, lag = 10, plot = FALSE)


## ------------------------------------------------------------------------
# aics
c(AIC(model10),AIC(model01))
# bics
c(BIC(model10),BIC(model01))


## ----tscv1, echo=TRUE----------------------------------------------------
# end of training set, 2000Q4
n.end <- 2000.75

# test set: 2001Q1 - 2012Q4
# set matrix for storage, 48 obs in test set
pred <- matrix(rep(NA,144),48,3)
# loop
for(i in 1:48){
  tmp0 <- 1970
  tmp1 <- n.end+(i-1)*1/4
  tmp <- window(rgdp.gr,tmp0,tmp1)
  pred[i,1] <- window(rgdp.gr,tmp1+1/4,tmp1+1/4) # actual
  # compute forecasts
  pred[i,2] <- forecast(Arima(tmp,order=c(1,0,0)),h=1)$mean # AR(1)
  pred[i,3] <- forecast(Arima(tmp,order=c(0,0,1)),h=1)$mean # MA(1)
}


## ---- echo=F, fig.height=4.4---------------------------------------------
# plot forecasts
fcast.all <- ts(pred, start=2001, frequency=4, names=c("Actual","AR(1)","MA(1)"))
autoplot(fcast.all) +
  theme(legend.position = "bottom")


## ----tscv2, echo=TRUE----------------------------------------------------
# compute rmse
rmse <- rep(NA,2)
for(m in 1:2){rmse[m] <- sqrt(mean((pred[,1]-pred[,1+m])^2))}
# display rmse
rmse

