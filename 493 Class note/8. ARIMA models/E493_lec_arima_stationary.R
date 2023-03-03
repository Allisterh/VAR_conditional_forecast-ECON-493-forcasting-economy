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
# plot index
autoplot(dj) + ggtitle("Dow Jones Index") + 
  xlab("Day") + ylab("")


## ---- fig.height=3.5-----------------------------------------------------
# plot first difference of index
autoplot(diff(dj)) + ggtitle("Change in Dow Jones Index") + 
  xlab("Day") + ylab("")


## ---- fig.height=3.5-----------------------------------------------------
# plot strikes
autoplot(strikes) + ggtitle("Number of strikes") + 
  xlab("Year") + ylab("")


## ---- fig.height=3.5-----------------------------------------------------
# plot new house sales
autoplot(hsales) + ggtitle("Total sales") + 
  xlab("Year") + ylab("")


## ---- echo=FALSE---------------------------------------------------------
# read quarterly data
data <- read.csv("data/covid19.csv", header = TRUE)
# get time series
ab.conf <- data[data$prname=="Alberta", c("date","numconf")]
t.start <- which(ab.conf$date=="15-03-2020")
t.end <- nrow(ab.conf)
ab.conf.ts <- ts(ab.conf$numconf[t.start:t.end])

## ---- fig.height=3.5-----------------------------------------------------
# plot COVID-19 in Alberta, total
autoplot(ab.conf.ts) + xlab("Year") + ylab(" ") 


## ---- fig.height=3.5-----------------------------------------------------
# plot COVID-19 in Alberta, new cases
autoplot(diff(ab.conf.ts)) + xlab("Year") + ylab(" ") 


## ---- fig.height=3.5-----------------------------------------------------
# plot quarterly beer production
autoplot(window(ausbeer, start = 1992)) + 
  xlab("Year") + ylab("megalitres") 


## ---- fig.height=3.5-----------------------------------------------------
# plot index
autoplot(dj) + ggtitle("Dow Jones Index") + 
  xlab("Day") + ylab(" ") 


## ---- fig.height=3.5-----------------------------------------------------
# plot ACF of index
ggAcf(dj)


## ---- fig.height=3.5-----------------------------------------------------
# plot first difference of index
autoplot(diff(dj)) + ggtitle("Change in Dow Jones Index") + 
  xlab("Day") + ylab(" ") 


## ---- fig.height=3.5-----------------------------------------------------
# plot ACF of first difference
ggAcf(diff(dj))


## ---- fig.height=3.5-----------------------------------------------------
# plot electricity prices
autoplot(usmelec)


## ---- fig.height=3.5-----------------------------------------------------
# take logs
autoplot(log(usmelec))


## ---- fig.height=3.5-----------------------------------------------------
# plot log seasonal-difference
autoplot(diff(log(usmelec),lag = 12))


## ---- fig.height=3.5-----------------------------------------------------
# plot log seasonal-difference, difference
autoplot(diff(diff(log(usmelec), lag = 12), lag = 1))


## ----arp, echo=FALSE-----------------------------------------------------
set.seed(1)
y1.ar <- 4 + arima.sim(list(ar = 0.5), n = 100)
y2.ar <- 10 + arima.sim(list(ar = 0.8), n = 100)
y3.ar <- 5 + arima.sim(list(ar = c(1.3, -0.7)), n = 100)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(y1.ar) + ylab("")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(y2.ar) + ylab("")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(y3.ar) + ylab("")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggAcf(y1.ar,main="")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggAcf(y2.ar,main="")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggAcf(y3.ar,main="")


## ---- echo=FALSE---------------------------------------------------------
set.seed(2)
y1.ma <- 4 + arima.sim(list(ma = 0.8), n = 100)
y2.ma <- arima.sim(list(ma = c(-1, 0.8)), n = 100)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(y1.ma) + ylab("")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
autoplot(y2.ma) + ylab("")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggAcf(y1.ma,main="")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
ggAcf(y2.ma,main="")


## ---- echo=FALSE---------------------------------------------------------
p1 <- ggAcf(y1.ar,main="")
p2 <- ggPacf(y1.ar,main="")
grid.arrange(p1,p2,ncol=2)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
p1 <- ggAcf(y2.ar,main="")
p2 <- ggPacf(y2.ar,main="")
grid.arrange(p1,p2,ncol=2)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
p1 <- ggAcf(y3.ar,main="")
p2 <- ggPacf(y3.ar,main="")
grid.arrange(p1,p2,ncol=2)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
p1 <- ggAcf(y1.ma,main="")
p2 <- ggPacf(y1.ma,main="")
grid.arrange(p1,p2,ncol=2)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------
p1 <- ggAcf(y2.ma,main="")
p2 <- ggPacf(y2.ma,main="")
grid.arrange(p1,p2,ncol=2)


## ---- fig.height=3.5-----------------------------------------------------
autoplot(uschange[,"Consumption"]) +
    xlab("Year") + ylab("Quarterly percentage change")


## ----usconsumptionacf, fig.height=3.5------------------------------------
p1 <-  ggAcf(uschange[,"Consumption"], main = "")
p2 <- ggPacf(uschange[,"Consumption"], main = "")
grid.arrange(p1, p2, ncol = 2)


## ---- echo=TRUE, fig.height=4.4------------------------------------------
# consumption time series
ggtsdisplay(uschange[,"Consumption"])


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# fit AR(1) model
(fit <- Arima(uschange[,"Consumption"], order = c(1,0,0)))


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit, lag = 10, test = FALSE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit, lag = 10, plot = FALSE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# fit AR(3) model
(fit2 <- Arima(uschange[,"Consumption"], order = c(3,0,0)))


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit2, lag = 10, test = FALSE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
# check residuals
checkresiduals(fit2, lag = 10, plot = FALSE)


## ---- echo=TRUE, fig.height=3.5------------------------------------------
fit <- Arima(y2.ar, order = c(1,0,0))
autoplot(forecast(fit, h = 50))


## ---- echo=TRUE, fig.height=3.5------------------------------------------
fit <- Arima(y3.ar, order = c(2,0,0))
autoplot(forecast(fit, h = 50))


## ---- echo=TRUE, fig.height=3.5------------------------------------------
fit <- Arima(y1.ma, order = c(0,0,1))
autoplot(forecast(fit, h = 50)) 


## ---- echo=TRUE, fig.height=3.5------------------------------------------
fit2 <- Arima(uschange[,"Consumption"], order = c(3,0,0))
autoplot(forecast(fit2)) 

