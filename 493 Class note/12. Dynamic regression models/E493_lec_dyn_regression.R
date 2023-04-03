----setup include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(fpp2)
library(urca)
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



## ----usconsump, fig.height=4.4-------------------------------------------
autoplot(uschange[,1:5], facets = TRUE, colour=TRUE) +
  ylab("") + xlab("Year") + guides(colour="none")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggplot(aes(x=Income,y=Consumption), data=as.data.frame(uschange)) +
  geom_point() +
  ggtitle("Quarterly changes in US consumption and personal income")


## ----usconsump2, fig.height=3.5------------------------------------------
# fit regression
(fit <- auto.arima(uschange[,1], xreg=uschange[,2]))


## ---- fig.height=3.5-----------------------------------------------------
ggtsdisplay(residuals(fit, type='regression'),
  main="Regression errors")


## ---- fig.height=3.5-----------------------------------------------------
ggtsdisplay(residuals(fit, type='response'),
  main="ARIMA errors")


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(fit, test=FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(fit, plot=FALSE)


## ----usconsump3, echo=FALSE , fig.height=3.5-----------------------------
fcast <- forecast(fit,xreg=rep(mean(uschange[,2]),8), h=8)
autoplot(fcast) + xlab("Year") + ylab("Percentage change") +
  ggtitle("Forecasts from regression with ARIMA(1,0,2) errors")


## ---- fig.height=3.5-----------------------------------------------------
# trend stationary process
yt <- ts(2 + .1*seq(1:250) + rnorm(250))
autoplot(yt)


## ---- fig.height=3.5-----------------------------------------------------
# trend stationary process
autoplot(resid(tslm(yt ~ trend)))


## ---- fig.height=3.5-----------------------------------------------------
# random walk
yt <- ts(cumsum(rnorm(250)))
autoplot(yt)


## ---- fig.height=3.5-----------------------------------------------------
# random walk
autoplot(diff(yt))


## ---- fig.height=3.5-----------------------------------------------------
# random walk w/ drift
yt <- ts(cumsum(.2 + rnorm(250)))
autoplot(yt)


## ---- fig.height=3.5-----------------------------------------------------
# random walk w/ drift
autoplot(diff(yt))


## ---- fig.height=3.5-----------------------------------------------------
autoplot(austa) + xlab("Year") + ylab("millions of people") +
  ggtitle("Total annual international visitors to Australia")


## ------------------------------------------------------------------------
trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend))


## ----austaparams, echo=FALSE---------------------------------------------
phi1 <- coef(fit1)['ar1']
phi2 <- coef(fit1)['ar2']
intercept <- coef(fit1)['intercept']
slope <- coef(fit1)['xreg']
sigma2 <- fit1$sigma2


## ------------------------------------------------------------------------
# 
(fit2 <- auto.arima(austa, d=1))


## ----austaparams2, cache=TRUE, echo=FALSE--------------------------------
drift <- coef(fit2)['drift']
theta1 <- coef(fit2)['ma1']
sigma2 <- fit2$sigma2


## ---- echo=FALSE, fig.height=2.4-----------------------------------------
autoplot(forecast(fit1, xreg=length(austa) + 1:10)) +
  xlab("Year") + ylab("") +
  ggtitle("Forecasts from linear trend with AR(2) error")


## ---- echo=FALSE, fig.height=2.4-----------------------------------------
autoplot(forecast(fit2)) +
  xlab("Year") + ylab("") +
  ggtitle("Forecasts from ARIMA(0,1,1) with drift")


## ----echo=FALSE, fig.height=4.4------------------------------------------
fc1 <- forecast(fit1, xreg=length(austa) + 1:10)
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")


## ---- echo=TRUE----------------------------------------------------------
# standard normal quantiles
qnorm(c(0.01,0.05,0.10))
# DF asymptotic critical values (requires urca package)
qunitroot(c(.01,.05,.1), trend="nc", statistic="t")
qunitroot(c(.01,.05,.1), trend="c", statistic="t")
qunitroot(c(.01,.05,.1), trend="ct", statistic="t")


## ---- fig.height=3.5-----------------------------------------------------
# plot series
autoplot(austa) + xlab("Year") + ylab("millions of people") 


## ---- eval=F-------------------------------------------------------------
## # ADF test, levels
## test1 <- ur.df(y = austa, type = 'trend', selectlags = "AIC", lags = 8)
## summary(test1)


## ---- fig.height=3.5-----------------------------------------------------
# plot series
autoplot(diff(austa)) + xlab("Year") + ylab("millions of people") 


## ---- eval=F, fig.height=3.5---------------------------------------------
## # ADF test, first diff
## test2 <- ur.df(y = diff(austa), type = 'drift', selectlags = "AIC", lags = 8)
## summary(test2)


## ----echo=TRUE, fig.height=3.5-------------------------------------------
# plot
autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") + ggtitle("Insurance advertising and quotations")


## ------------------------------------------------------------------------
# generate lagged predictors
Advert <- 
  cbind(
    AdLag0 = insurance[,"TV.advert"],
    AdLag1 = lag(insurance[,"TV.advert"],-1),
    AdLag2 = lag(insurance[,"TV.advert"],-2),
    AdLag3 = lag(insurance[,"TV.advert"],-3)
  )
Advert <- head(Advert, NROW(insurance))
# restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], stationary=TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], stationary=TRUE)
# evaluate fit
c(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc)


## ----tvadvertagain-------------------------------------------------------
# fit model
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], stationary=TRUE))


## ----tvadvertparam, echo=FALSE-------------------------------------------
# Store coefficients
phi1 <- coef(fit)['ar1']
phi2 <- coef(fit)['ar2']
phi3 <- coef(fit)['ar3']
intercept <- coef(fit)['intercept']
gamma0 <- coef(fit)['AdLag0']
gamma1 <- coef(fit)['AdLag1']


## ---- fig.height=3-------------------------------------------------------
fc <- forecast(fit, h = 20, 
               xreg = cbind(
                 AdLag0 = rep(10,20), 
                 AdLag1 = c(Advert[40,1], rep(10,19))
               ))
autoplot(fc)


## ---- fig.height=3-------------------------------------------------------
fc <- forecast(fit, h = 20, 
               xreg = cbind(
                 AdLag0 = rep(8,20), 
                 AdLag1 = c(Advert[40,1], rep(8,19))
               ))
autoplot(fc)


## ---- fig.height=3-------------------------------------------------------
fc <- forecast(fit, h = 20, 
               xreg = cbind(
                 AdLag0 = rep(6,20), 
                 AdLag1 = c(Advert[40,1], rep(6,19))
               ))
autoplot(fc)


## ------------------------------------------------------------------------
# simulate two rw
yt <- ts(cumsum(rnorm(250)))
xt <- ts(cumsum(rnorm(250)))

# regression (1) in levels
reg1 <- tslm(yt ~ xt)

# regression (2) in differences
reg2 <- tslm(diff(yt) ~ diff(xt))


## ------------------------------------------------------------------------
# estimates
coeftest(reg1)
# R2
summary(reg1)$r.squared


## ------------------------------------------------------------------------
# estimates
coeftest(reg2)
# R2
summary(reg2)$r.squared


## ---- fig.height=3.5-----------------------------------------------------
# plot time series
autoplot(yt, series="yt") +
  autolayer(xt, series="xt") + xlab("") + ylab("")


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(reg1, test = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(reg2, test = FALSE)


## ------------------------------------------------------------------------
# simulate two rw
yt <- ts(cumsum(.5 + rnorm(250)))
xt <- ts(cumsum(.3 + rnorm(250)))

# regression (3) in levels
reg3 <- tslm(yt ~ xt)

# regression (4) in differences
reg4 <- tslm(diff(yt) ~ diff(xt))


## ------------------------------------------------------------------------
# estimates
coeftest(reg3)
# R2
summary(reg3)$r.squared


## ------------------------------------------------------------------------
# estimates
coeftest(reg4)
# R2
summary(reg4)$r.squared


## ---- fig.height=3.5-----------------------------------------------------
# plot time series
autoplot(yt, series="yt") +
  autolayer(xt, series="xt") + xlab("") + ylab("")


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(reg3, test = FALSE)


## ---- fig.height=3.5-----------------------------------------------------
# check residuals
checkresiduals(reg4, test = FALSE)

