## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(fpp2)
library(vars)
library(sandwich)
library(lmtest)

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



## ---- echo=TRUE-------------------------------------------------------
# read quarterly data
data <- read.csv("data/us_macro_quarterly.csv", header = TRUE)

# get time series
gdpgr <- 400*diff(log(data$GDPC96)) 
tsprd <- data$GS10-data$TB3MS
tsprd <- tsprd[-1]

# full sample
vardata0 <- ts(cbind(gdpgr,tsprd), start=c(1957,2), frequency=4)
vardata0 <- window(vardata0, start=c(1984,1), end=c(2012,4))


## ---- echo=TRUE, cache=TRUE, fig.height=3.5---------------------------
# plot data
autoplot(vardata0, facets = TRUE, colour=TRUE) + 
  ylab("") + xlab("") + guides(colour="none")


## ---- echo=TRUE-------------------------------------------------------
# find optimal number of lags
VARselect(vardata0, lag.max = 8, type="const")[["selection"]]


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# estimate VAR(2)
model0 <- VAR(vardata0, p = 2, type = "const")
serial.test(model0, lags.pt = 10, type = "PT.asymptotic")


## ---- echo=TRUE-------------------------------------------------------
# forecasts
fcast0 <- forecast(model0, h=2)
fcast0


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# forecasts
autoplot(forecast(model0, h=10)) + xlab("")


## ---- echo=TRUE-------------------------------------------------------
# test for Granger causality
causality(model0,cause="tsprd")$Granger
causality(model0,cause="gdpgr")$Granger


## ----cv11, cache=TRUE, echo=FALSE, fig.height=3.5---------------------
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,28),ylim=c(0,1),
       xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
i <- 1
for(j in 1:10)
{
  test <- (16+j):26
  train <- 1:(15+j)
  arrows(0,1-j/20,27,1-j/20,0.05)
  points(train,rep(1-j/20,length(train)),pch=19,col=my_colors[2])
  if(length(test) >= i)
    points(test[i], 1-j/20, pch=19, col=my_colors[3])
  if(length(test) >= i)
    points(test[-i], rep(1-j/20,length(test)-1), pch=19, col="gray")
  else
    points(test, rep(1-j/20,length(test)), pch=19, col="gray")
}
text(28,.95,"time")
text(10,1,"Training data",col=my_colors[2])
text(21,1,"Test data",col=my_colors[3])


## ----cv2, cache=TRUE, echo=FALSE, fig.height=3.5----------------------
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,28),ylim=c(0,1),
       xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
i <- 1
for(j in 1:10)
{
  test <- (16+j):26
  train <- 1:(15+j)
  arrows(0,1-j/20,27,1-j/20,0.05)
  points(train,rep(1-j/20,length(train)),pch=19, col=my_colors[2])
  if(length(test) >= i)
    points(test[i], 1-j/20, pch=19, col=my_colors[3])
  if(length(test) >= i)
    points(test[-i], rep(1-j/20,length(test)-1), pch=19, col="gray")
  else
    points(test, rep(1-j/20,length(test)), pch=19, col="gray")
  if(j>1){points(train[1:(j-1)],rep(1-j/20,(j-1)),pch=19, col="gray")}
}
text(28,.95,"time")
text(10,1,"Training data",col=my_colors[2])
text(21,1,"Test data",col=my_colors[3])


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# simulate data
y1 <- arima.sim(list(ar = 0.5), n = 200)
autoplot(y1) + xlab("") + ylab("")


## ---- echo=FALSE, fig.height=3.5--------------------------------------
# loop
pred.rec <- matrix(rep(NA,450),150,3)
pred.rol <- matrix(rep(NA,450),150,3)


## ---- echo=TRUE, fig.height=3.5---------------------------------------
n.end <- 50
for(i in 1:150){
  # recursive forecast
  tmp0 <- 1
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rec[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rec[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rec[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
  # rolling forecast
  tmp0 <- i
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rol[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rol[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rol[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
}


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# recursive vs rolling
cbind(pred.rec[,1],pred.rol[,1])[1:5,]


## ---- echo=F, fig.height=4.4------------------------------------------
# plot forecasts
fcst.all <- ts(
  cbind(pred.rec[,2],pred.rec[,3],pred.rol[,3]), 
  start=n.end+1, 
  names=c("Actual","Recursive","Rolling")
)
autoplot(y1) + autolayer(fcst.all) + xlab("") + ylab("") +
  theme(legend.position = "bottom")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# display rmse
fit.rec <- pred.rec[,2] - pred.rec[,3]
fit.rol <- pred.rol[,2] - pred.rol[,3]
rmse.rec <- sqrt(mean(fit.rec^2, na.rm=TRUE))
rmse.rol <- sqrt(mean(fit.rol^2, na.rm=TRUE))
cbind(rmse.rec,rmse.rol)


## ---- echo=FALSE, fig.height=3.5--------------------------------------
# simulate structural break
tc <- rep(0,200)
tc[50] <-  1
ls <- filter(tc, filter=1, method="recursive")
autoplot(ls) + xlab("") + ylab("")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# data with one break at t = 100
y1 <- 4*ls + arima.sim(list(ar = 0.5), n = 200)
autoplot(y1) + xlab("") + ylab("")


## ---- echo=TRUE-------------------------------------------------------
# pre-break model
(Arima(window(y1,1,100), order=c(1,0,0)))


## ---- echo=TRUE-------------------------------------------------------
# post-break model
(Arima(window(y1,101,200), order=c(1,0,0)))


## ---- echo=TRUE-------------------------------------------------------
# full sample model
(Arima(y1, order=c(1,0,0)))


## ---- echo=FALSE, fig.height=3.5--------------------------------------
# loop
pred.rec <- matrix(rep(NA,450),150,3)
pred.rol <- matrix(rep(NA,450),150,3)


## ---- echo=TRUE, fig.height=3.5---------------------------------------
n.end <- 50
for(i in 1:150){
  # recursive forecast
  tmp0 <- 1
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rec[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rec[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rec[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
  # rolling forecast
  tmp0 <- i
  tmp1 <- n.end+(i-1)
  tmp <- window(y1,tmp0,tmp1)
  pred.rol[i,1] <- tmp1-tmp0+1 # obs for estimation
  pred.rol[i,2] <- window(y1,tmp1+1,tmp1+1) # actual 
  pred.rol[i,3] <- forecast(Arima(tmp, order=c(1,0,0)), h=1)$mean
}


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# recursive vs rolling
cbind(pred.rec[,1],pred.rol[,1])[1:5,]


## ---- echo=F, fig.height=4.4------------------------------------------
# plot forecasts
fcst.all <- ts(
  cbind(pred.rec[,2],pred.rec[,3],pred.rol[,3]), 
  start=n.end+1, 
  names=c("Actual","Recursive","Rolling")
)
autoplot(y1) + autolayer(fcst.all) + xlab("") + ylab("") +
  theme(legend.position = "bottom")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# display rmse
fit.rec <- pred.rec[,2] - pred.rec[,3]
fit.rol <- pred.rol[,2] - pred.rol[,3]
rmse.rec <- sqrt(mean(fit.rec^2, na.rm=TRUE))
rmse.rol <- sqrt(mean(fit.rol^2, na.rm=TRUE))
cbind(rmse.rec,rmse.rol)


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# beer data
beer1 <- window(ausbeer, start=1992,end=c(2009,4))
autoplot(beer1) + xlab("") + ylab("Megalitres")


## ----beercv1, echo=TRUE-----------------------------------------------
# end of training set, 2004Q4
n.end <- 2004.75 

# set matrix for storage, 20 obs in test set
pred <- matrix(rep(NA,80),20,4)
# loop
for(i in 1:20){
  tmp0 <- 1992
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(beer1,tmp0,tmp1)
  pred[i,1] <- window(beer1,tmp1+.25,tmp1+.25) # actual 
  # compute forecasts
  pred[i,2] <- meanf(tmp, h=1)$mean # mean forecast
  pred[i,3] <- snaive(tmp, h=1)$mean # seasonal last value
  pred[i,4] <- forecast(tslm(tmp~trend+season), h=1)$mean # trend + seasonal dummies
}


## ---- echo=F, fig.height=4.4------------------------------------------
# plot forecasts
fcast.all <- ts(
  pred, 
  start=2005, 
  frequency=4, 
  names=c("Actual","Mean","SNaive","STrend")
)
autoplot(fcast.all) + ylab("") + xlab("") +
  theme(legend.position = "bottom")


## ---------------------------------------------------------------------
beerfit1 <- pred[,1] - pred[,2]
e1 <- ts(beerfit1, start=2005, frequency=4)
# robust tests only need for h>1
test.eq <- tslm(e1~1)
coeftest(test.eq)


## ---------------------------------------------------------------------
# robust tests only need for h>1
test.eq <- Arima(e1,order=c(1,0,0))
coeftest(test.eq)


## ---------------------------------------------------------------------
# robust tests only need for h>1
test.eq <- tslm(e1~season)
coeftest(test.eq)


## ---------------------------------------------------------------------
beerfit2 <- pred[,1] - pred[,3]
e2 <- ts(beerfit2, start=2005, frequency=4)
# robust tests only need for h>1
test.eq <- tslm(e2~1)
coeftest(test.eq)


## ---------------------------------------------------------------------
# robust tests only need for h>1
test.eq <- Arima(e2,order=c(1,0,0))
coeftest(test.eq)


## ---------------------------------------------------------------------
# robust tests only need for h>1
test.eq <- tslm(e2~season)
coeftest(test.eq)


## ---- echo=FALSE, fig.height=3.5--------------------------------------
# plot prediction errors
error.diff = data.frame(date=time(e2),error=beerfit1^2 - beerfit2^2)
ggplot(error.diff) +
  geom_col(aes(x=date, y=error), fill="steelblue") + xlab("") + ylab("") +
  ggtitle("Sample loss differences, quadratic loss")


## ---------------------------------------------------------------------
# compare model 1 (mean) with model 2 (seasonal naive)
# compute Diebold-Mariano statistic
dm.test(beerfit1, beerfit2, h=1, power=2)


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# gold price
autoplot(gold) + ylab("") + xlab("")


## ---- echo=T, fig.height=4--------------------------------------------
# fill in missing values
gold2 <- na.interp(gold)
autoplot(gold2, series="Interpolated") + autolayer(gold, series="Original") +
  theme(legend.position = "bottom")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# gold price
autoplot(gold) + ylab("") + xlab("")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# find outliers
tsoutliers(gold)


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# remove outliers
gold2 <- tsclean(gold) 
autoplot(gold2) + ylab("") + xlab("")


## ---- echo=TRUE-------------------------------------------------------
# read quarterly data
data <- read.csv("data/recent_macro_data.csv", header = TRUE)
tail(data, 4)


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# rgdp canada
rgdp_ca <- ts(data$rgdp_ca, start=c(1980,1), freq=4)
autoplot(rgdp_ca) + ylab("") + xlab("") + ggtitle("CA real GDP")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# rgdp canada, growth
rgdp_g_ca <- ts(400*diff(log(data$rgdp_ca)), start=c(1980,2), freq=4)
autoplot(rgdp_g_ca) + ylab("") + xlab("") + ggtitle("CA real GDP, growth rate")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# unem rate canada
unem_ca <- ts(data$unem_ca, start=c(1980,1), freq=4)
autoplot(unem_ca) + ylab("") + xlab("") + ggtitle("CA unemployment rate")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# unem rate canada, change
unem_d_ca <- ts(diff(data$unem_ca), start=c(1980,2), freq=4)
autoplot(unem_d_ca) + ylab("") + xlab("") + ggtitle("CA unemployment rate, change")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# cpi canada
cpi_ca <- ts(data$cpi_ca, start=c(1980,1), freq=4)
# inflation rate canada
inf_ca <- ts(400*diff(log(cpi_ca)), start=c(1980,2), freq=4)
autoplot(inf_ca) + ylab("") + xlab("") + ggtitle("CA inflation")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# rgdp us
rgdp_us <- ts(data$rgdp_us, start=c(1980,1), freq=4)
autoplot(rgdp_us) + ylab("") + xlab("") + ggtitle("US real GDP")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# rgdp us, growth
rgdp_g_us <- ts(400*diff(log(data$rgdp_us)), start=c(1980,2), freq=4)
autoplot(rgdp_g_us) + ylab("") + xlab("") + ggtitle("US real GDP, growth rate")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# unem rate us
unem_us <- ts(data$unem_us, start=c(1980,1), freq=4)
autoplot(unem_us) + ylab("") + xlab("") + ggtitle("US unemployment rate")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# unem rate us, change
unem_d_us <- ts(diff(data$unem_us), start=c(1980,2), freq=4)
autoplot(unem_d_us) + ylab("") + xlab("") + ggtitle("US unemployment rate, change")


## ---- echo=FALSE, cache=TRUE, fig.height=4----------------------------
# cpi us
cpi_us <- ts(data$cpi_us, start=c(1980,1), freq=4)
# inflation rate us
inf_us <- ts(400*diff(log(cpi_us)), start=c(1980,2), freq=4)
autoplot(inf_us) + ylab("") + xlab("") + ggtitle("US inflation")


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# read quarterly data
data <- read.csv("data/us_macro_quarterly.csv", header = TRUE)

# get time series
gdpgr <- 
  ts(
    400*diff(log(data$GDPC96)), 
    start = c(1957,2), 
    freq = 4
  )
gdpgr0 <- window(gdpgr, start = 1968)


## ---- echo=TRUE-------------------------------------------------------
# plot data
autoplot(gdpgr0) + xlab("") + ylab("")


## ---- echo=TRUE-------------------------------------------------------
# loop
n.end <- 1979.75 # 1979Q4
pred0 <- matrix(rep(NA,136*2),136,2)
for(i in 1:136){
  # recursive forecast
  tmp0 <- 1968
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(gdpgr0, tmp0, tmp1)
  pred0[i,1] <- window(gdpgr0, tmp1+.25, tmp1+.25) # actual 
  pred0[i,2] <- forecast(Arima(tmp, order = c(2,0,0)), h = 1)$mean
}


## ---- echo=FALSE, fig.height=4----------------------------------------
# plot forecasts
fcst <- ts(pred0, start = c(1980,1), freq = 4, names = c("Actual","AR(2)"))
# plot
autoplot(gdpgr0) + autolayer(fcst) + xlab("") + ylab("") 


## ---- echo=FALSE, fig.height=4----------------------------------------
# plot forecast errors
pred0.err <- ts(pred0[,1]-pred0[,2], start = c(1980,1), freq = 4)
# plot
autoplot(pred0.err) + xlab("") + ylab("") + ggtitle("Prediction errors")


## ---- echo=FALSE, fig.height=4----------------------------------------
# plot forecast errors
pred0.err.df <- 
  data.frame(
    date = time(pred0.err),
    error = pred0.err
  )
# plot
ggplot(pred0.err.df) + 
  geom_col(aes(x = date, y = error), fill = "steelblue") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Prediction errors")


## ---- echo=TRUE-------------------------------------------------------
# NBER peaks and troughs
peak <- c(1980+0/12, 1981+6/12, 1990+6/12, 2001+2/12, 2007+11/12)
trough <- c(1980+6/12, 1982+10/12, 1991+2/12, 2001+10/12, 2009+5/12)
recessions.df <- data.frame(Peak = peak, Trough = trough)


## ---- echo=F, fig.height=4--------------------------------------------
ggplot(pred0.err.df) + 
  geom_rect(
    data = recessions.df, 
    aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
    fill = 'grey', 
    alpha = 0.4
  ) +
  geom_col(
    aes(x = date, y = error), fill = "steelblue") + 
  xlab("") + 
  ylab("") + 
  theme_bw() + 
  ggtitle("Prediction errors")


## ---- echo=FALSE------------------------------------------------------
# read NBER dates
nber <- read.csv("data/USREC.csv", header = TRUE)
nber.m <- ts(nber$USREC, start = c(1967,6), freq = 12)

nber.m <- window(nber.m, start = c(1968,1), end = c(2013,12))
nber.q <- aggregate(nber.m, nfrequency = 4)

for (i in 1:length(nber.q)){ if (nber.q[i]>0){nber.q[i]=1} }


## ---- echo=TRUE, fig.height=3.5---------------------------------------
# test unbiasedness
nber0 <- window(nber.q, start = c(1980,1))
coeftest(tslm(pred0.err ~ nber0))


## ---- echo=TRUE-------------------------------------------------------
# loop
n.end <- 1979.75 # 1979Q4
pred1 <- matrix(rep(NA,136*3),136,3)
for(i in 1:136){
  # recursive forecast
  tmp0 <- 1968
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(gdpgr0, tmp0, tmp1)
  reg.est <- window(nber.q, tmp0, tmp1)
  reg.new <- window(nber.q, tmp1+.25, tmp1+.25)
  pred1[i,1] <- window(gdpgr0, tmp1+.25, tmp1+.25) # actual 
  pred1[i,2] <- forecast(Arima(tmp, order = c(2,0,0)),h = 1)$mean
  pred1[i,3] <- forecast(Arima(tmp, order = c(2,0,0), xreg = reg.est),
                         h = 1, xreg = reg.new)$mean
}


## ---- echo=F, fig.height=4--------------------------------------------
# plot forecasts
fcst <- ts(pred1, start = c(1980,1), freq = 4, names = c("Actual","AR(2)","AR(2)+NBER"))
# plot
autoplot(gdpgr0) + 
  autolayer(fcst) + 
  xlab("") + 
  ylab("") 


## ---- echo=T----------------------------------------------------------
# get prediction errors
pred1.err <- ts(pred1[,1] - pred1[,3], start = c(1980,1), freq = 4)
pred1.err.df <- data.frame(date = time(pred1.err), error = pred1.err)

# compute rmse (h=1)
fit1 <- pred1[,1] - pred1[,2]
fit2 <- pred1[,1] - pred1[,3]
rmse1 <- sqrt(mean(fit1^2, na.rm=TRUE))
rmse2 <- sqrt(mean(fit2^2, na.rm=TRUE))

# display rmse
cbind(rmse1,rmse2)


## ---- echo=TRUE-------------------------------------------------------
# test unbiasedness 
coeftest(tslm(pred1.err ~ nber0))

