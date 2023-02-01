## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(fpp2)

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



## ----ausbeer, echo=FALSE-------------------------------------------------------
beer2 <- window(ausbeer, start = 1992)
autoplot(beer2) +
  xlab("Year") + ylab("megalitres") +
    ggtitle("Australian quarterly beer production")


## ----dj, echo=FALSE------------------------------------------------------------
autoplot(dj) + xlab("Day") +
  ggtitle("Dow-Jones index") + ylab("")


## ---- eval=FALSE---------------------------------------------------------------
## # y contains the time series
## # h is the forecast horizon
## meanf(y, h)


## ---- eval=FALSE---------------------------------------------------------------
## # y contains the time series
## # h is the forecast horizon
## naive(y, h) # alternative rwf(y, h)


## ---- eval=FALSE---------------------------------------------------------------
## # y contains the time series
## # h is the forecast horizon
## snaive(y, h)


## ---- eval=FALSE---------------------------------------------------------------
## # y contains the time series
## # h is the forecast horizon
## rwf(y, h, drift = TRUE)


## ---- eval=FALSE---------------------------------------------------------------
## # get data
## beer2 <- window(ausbeer, start = 1992, end = c(2007,4))
## # get forecasts
## fcst.m <- meanf(beer2, h = 10)
## fcst.n <- naive(beer2, h = 10)
## fcst.s <- snaive(beer2, h = 10)
## # plot
## autoplot(beer2) +
##   autolayer(fcst.m, PI = FALSE, series = "Mean") +
##   autolayer(fcst.n, PI = FALSE, series = "Naïve") +
##   autolayer(fcst.s, PI = FALSE, series = "Seasonal naïve") +
##   ggtitle("Forecasts for quarterly beer production") +
##   xlab("Year") + ylab("Megalitres") +
##   guides(colour = guide_legend(title = "Forecast"))


## ---- echo=FALSE---------------------------------------------------------------
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))
autoplot(beer2) +
  autolayer(meanf(beer2, h = 10), PI = FALSE, series = "Mean") +
  autolayer(naive(beer2, h = 10), PI = FALSE, series = "Naïve") +
  autolayer(snaive(beer2, h = 10), PI = FALSE, series = "Seasonal naïve") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title = "Forecast"))


## ---- echo=FALSE---------------------------------------------------------------
# set training data to first 250 days
dj2 <- window(dj, end = 250)
autoplot(dj2) +
  autolayer(meanf(dj2, h = 42), PI = FALSE, series = "Mean") +
  autolayer(rwf(dj2, h = 42), PI = FALSE, series = "Naïve") +
  autolayer(rwf(dj2, drift = TRUE, h = 42), PI = FALSE, series = "Drift") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") +
  xlab("Day") + ylab("") +
  guides(colour = guide_legend(title = "Forecast"))


## ----elec, echo=FALSE----------------------------------------------------------
autoplot(elec) + xlab("Year") + ylab("") +
  ggtitle("Australian electricity production")


## ----elec1, echo=FALSE---------------------------------------------------------
autoplot(elec^0.5) + xlab("Year") + ylab("") +
  ggtitle("Square root electricity production")


## ----elec3, echo=FALSE---------------------------------------------------------
autoplot(log(elec)) + xlab("Year") + ylab("") +
  ggtitle("Log electricity production")


## ----elec6, echo=TRUE----------------------------------------------------------
# box-cox
autoplot(BoxCox(elec, lambda = 1/3))


## ----elec7, echo=TRUE----------------------------------------------------------
# box-cox selection
BoxCox.lambda(elec)


## ----elec8,echo=TRUE-----------------------------------------------------------
# forecasting
fit <- snaive(elec, lambda = 1/3)
autoplot(fit)


## ----elec9,echo=TRUE-----------------------------------------------------------
# a closer look
autoplot(fit, include = 120)


## ----dj3, echo=F---------------------------------------------------------------
autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")


## ----dj4, echo=FALSE, warning=FALSE--------------------------------------------
fits <- fitted(naive(goog200))
autoplot(goog200, series = "Data") +
  autolayer(fits, series = "Fitted") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")


## ----dj5, echo=TRUE------------------------------------------------------------
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") 


## ----dj6, warning=FALSE--------------------------------------------------------
# histogram
gghistogram(res, add.normal = TRUE) + ggtitle("Histogram of residuals")


## ----dj7-----------------------------------------------------------------------
# acf of residuals
ggAcf(res) + ggtitle("ACF of residuals")


## ----dj9, echo=TRUE------------------------------------------------------------
# test
Box.test(res, lag = 10, fitdf = 0, type = "Lj")


## ---- echo=TRUE----------------------------------------------------------------
# useful function!
checkresiduals(naive(goog200), test = FALSE)


## ---- echo=TRUE----------------------------------------------------------------
# useful function!
checkresiduals(naive(goog200), plot = FALSE)


## ----traintest1, fig.height=1, echo=FALSE, cache=TRUE--------------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col=my_colors[2])
points(test,  test*0+0.5,  pch=19, col=my_colors[3])
text(26,0.5,"time")
text(10,1,"Training data",col=my_colors[2])
text(21,1,"Test data",col=my_colors[3])


## ----beeraccuracy, echo=FALSE--------------------------------------------------
beer2 <- window(ausbeer, start = 1992, end = c(2007,4))
beerfit1 <- meanf(beer2, h = 10)
beerfit2 <- rwf(beer2, h = 10)
beerfit3 <- snaive(beer2, h = 10)
tmp <- 
  cbind(
    Data = window(ausbeer, start = 1992),
    Mean = beerfit1[["mean"]],
    Naive = beerfit2[["mean"]],
    SeasonalNaive = beerfit3[["mean"]]
  )
# 
autoplot(tmp) + xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  scale_color_manual(
    values = c('#000000','#1b9e77','#d95f02','#7570b3'),
    breaks = c("Mean","Naive","SeasonalNaive"),
    name = "Forecast Method"
  )


## ---- echo=TRUE, eval=FALSE----------------------------------------------------
## beer2 <- window(ausbeer, start = 1992, end = c(2007,4)) # training set
## beer3 <- window(ausbeer, start = 2008) # test set
## beerfit1 <- meanf(beer2, h = 10)
## beerfit2 <- rwf(beer2, h = 10)
## beerfit3 <- snaive(beer2, h = 10)
## accuracy(beerfit1, beer3)
## accuracy(beerfit2, beer3)
## accuracy(beerfit3, beer3)


## ----beertable, echo=FALSE-----------------------------------------------------
beer3 <- window(ausbeer, start = 2008)
tab <- matrix(NA, ncol = 3, nrow = 3)
tab[1,] <- accuracy(beerfit1, beer3)[2,c(2,3,5)]
tab[2,] <- accuracy(beerfit2, beer3)[2,c(2,3,5)]
tab[3,] <- accuracy(beerfit3, beer3)[2,c(2,3,5)]
colnames(tab) <- c("RMSE","MAE","MAPE")
rownames(tab) <- c("Mean method", "Naïve method", "Seasonal naïve method")
knitr::kable(tab, digits = 2)


## ----traintest2, fig.height=1, echo=FALSE, cache=TRUE--------------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col=my_colors[2])
points(test,  test*0+0.5,  pch=19, col=my_colors[3])
text(26,0.5,"time")
text(10,1,"Training data",col=my_colors[2])
text(21,1,"Test data",col=my_colors[3])


## ----traintest3, fig.height=1, echo=FALSE, cache=TRUE--------------------------
train = 1:18
test = 19:24
par(mar=c(0,0,0,0))
plot(0,0,xlim=c(0,26),ylim=c(0,2),xaxt="n",yaxt="n",bty="n",xlab="",ylab="",type="n")
arrows(0,0.5,25,0.5,0.05)
points(train, train*0+0.5, pch=19, col=my_colors[2])
points(test,  test*0+0.5,  pch=19, col=my_colors[3])
text(26,0.5,"time")
text(10,1,"Training data",col=my_colors[2])
text(21,1,"Test data",col=my_colors[3])


## ----cv1, cache=TRUE, echo=FALSE, fig.height=3.5-------------------------------
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


## ----beercv1, echo=TRUE--------------------------------------------------------
# data and set up
beer1 <- window(ausbeer, start = 1992,end = c(2009,4))
n.end <- 2004.75 # 2004Q4
# set matrix for storage, 20 obs in test set
pred <- matrix(rep(NA,80),20,4)
# loop
for(i in 1:20){
  tmp0 <- 1992
  tmp1 <- n.end+(i-1)*.25
  tmp <- window(beer1, tmp0, tmp1)
  pred[i,1] <- window(beer1, tmp1+.25, tmp1+.25) # actual 
  # compute forecasts
  pred[i,2] <- meanf(tmp, h = 1)$mean
  pred[i,3] <- rwf(tmp, h = 1)$mean
  pred[i,4] <- snaive(tmp, h = 1)$mean
}


## ----beercv2, echo=TRUE--------------------------------------------------------
# compute rmse
beerfit1 <- pred[,1] - pred[,2]
beerfit2 <- pred[,1] - pred[,3]
beerfit3 <- pred[,1] - pred[,4]
rmse1 <- sqrt(mean(beerfit1^2, na.rm=TRUE))
rmse2 <- sqrt(mean(beerfit2^2, na.rm=TRUE))
rmse3 <- sqrt(mean(beerfit3^2, na.rm=TRUE))
# display rmse
cbind(rmse1,rmse2,rmse3)


## ----beercv3, echo=TRUE--------------------------------------------------------
# time series cross-validation function
beerfit1 <- tsCV(beer1, meanf, h = 1)
beerfit2 <- tsCV(beer1, rwf, h = 1)
beerfit3 <- tsCV(beer1, snaive, h = 1)

# compute rmse
rmse1 <- sqrt(mean(beerfit1[52:71]^2))
rmse2 <- sqrt(mean(beerfit2[52:71]^2))
rmse3 <- sqrt(mean(beerfit3[52:71]^2))

# display rmse
cbind(rmse1,rmse2,rmse3)


## ---- echo=TRUE, fig.height=3.5------------------------------------------------
# organize results
fcast.all <- ts(pred, start = 2005, freq = 4, names = c("Actual","Mean","Naive","SNaive"))
autoplot(fcast.all)


## ----djpi, echo=TRUE, cache=TRUE-----------------------------------------------
# compute sigma hat
res <- residuals(naive(goog200))
res_sd <- sqrt(mean(res^2, na.rm=TRUE))
# compute 05% interval
c(tail(goog200,1)) + 1.96 * res_sd * c(-1,1)


## ----djforecasts, echo=TRUE, cache=TRUE----------------------------------------
# predictions and prediction intervals
naive(goog200, level = 95)


## ----djforecastsplot, echo=TRUE, cache=TRUE------------------------------------
# plot prediction intervals
autoplot(naive(goog200))

