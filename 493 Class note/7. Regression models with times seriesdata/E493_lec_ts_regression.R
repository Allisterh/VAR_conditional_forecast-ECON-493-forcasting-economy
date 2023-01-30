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



## ----uschangedata, echo=FALSE--------------------------------------------------
quarters <- rownames(.preformat.ts(uschange))


## ----ConsInc, echo=FALSE-------------------------------------------------------
# plot consumption data
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")


## ----ConsInc2, echo=FALSE------------------------------------------------------
ggplot(aes(x=Income, y=Consumption), data=as.data.frame(uschange)) +
  ylab("Consumption (quarterly % change)") + 
  xlab("Income (quarterly % change)") +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', color = "darkorange") 


## ---- echo=TRUE----------------------------------------------------------------
fit.cons <- tslm(Consumption ~ Income, data = uschange)
coeftest(fit.cons)


## ----MultiPredictors, echo=FALSE, fig.height=4.5-------------------------------
autoplot(uschange[,1:5], facets = TRUE, colour=TRUE) +
  ylab("") + xlab("Year") + guides(colour = "none")


## ------------------------------------------------------------------------------
fit.consMR <- 
  tslm(
    Consumption ~ Income + Production + Unemployment + Savings,
    data = uschange
  )
coeftest(fit.consMR)


## ----usfitted1, echo=FALSE-----------------------------------------------------
autoplot(uschange[,'Consumption'], series = "Data") +
  autolayer(fitted(fit.consMR), series = "Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percentage change in US consumption expenditure") +
  guides(colour = guide_legend(title = " "))


## ----usfitted2, echo=FALSE-----------------------------------------------------
tmp <- 
  data.frame(
    "Data" = uschange[,"Consumption"],
    "Fitted" = fitted(fit.consMR)
  )
ggplot(aes(x = Data, y = Fitted), data = tmp) +
  geom_point(color = "grey40", alpha = 0.8) + 
  xlab("Fitted (predicted values)") +
  ylab("Data (actual values)") +
  ggtitle("Percentage change in US consumption expenditure") +
  geom_abline(intercept = 0, slope = 1)


## ---- echo=TRUE----------------------------------------------------------------
# check residuals
checkresiduals(fit.consMR, test = FALSE)


## ---- echo=TRUE----------------------------------------------------------------
# check residuals
checkresiduals(fit.consMR, plot=FALSE)


## ---- echo=FALSE---------------------------------------------------------------
beer <- window(ausbeer, start = 1992)
autoplot(beer) + xlab("Year") + ylab("megalitres") +
  ggtitle("Australian quarterly beer production")


## ---- echo=TRUE----------------------------------------------------------------
fit.beer <- tslm(beer ~ trend + season)
coeftest(fit.beer)


## ---- echo=FALSE---------------------------------------------------------------
autoplot(beer, series = "Data") +
  autolayer(fitted(fit.beer), series = "Fitted") +
  xlab("Year") + ylab("Megalitres") + 
  ggtitle("Australian quarterly beer production")


## ---- echo=FALSE---------------------------------------------------------------
tmp <- data.frame(
  "Data" = beer,
  "Fitted" = fitted(fit.beer)
)
ggplot(
  aes(x = Data, y = Fitted, colour = as.factor(cycle(beer))), 
  data = tmp
) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Australian quarterly beer production") +
  scale_colour_brewer(palette = "Dark2", name = "Quarter") +
  geom_abline(intercept = 0, slope = 1)


## ---- echo=TRUE----------------------------------------------------------------
# check residuals
checkresiduals(fit.beer, test = FALSE)


## ---- echo=TRUE----------------------------------------------------------------
# check residuals
checkresiduals(fit.beer, plot = FALSE)


## ---- echo=TRUE----------------------------------------------------------------
# plot forecasts
fcast <- forecast(fit.beer)
autoplot(fcast) + xlab("Year") + ylab("megalitres")


## ---- echo=FALSE---------------------------------------------------------------
data <- read.csv("data/googletrends_applepie_ca.csv", header = FALSE)
canpie <- ts(data$V2, start = c(2013,33), freq = 52.18)
autoplot(canpie) + ylab(" ") + 
  ggtitle("Google Trends: 'Apple Pie Recipe' in Canada")


## ---- echo=TRUE----------------------------------------------------------------
# Boston marathon times
autoplot(marathon) +
  xlab("Year") +  ylab("Winning times in minutes")


## ---- echo=TRUE----------------------------------------------------------------
marathon2 <- window(marathon, start = 1940)
autoplot(marathon2) +
  xlab("Year") +  ylab("Winning times in minutes")


## ---- echo=TRUE----------------------------------------------------------------
# linear trend
fit.lin <- tslm(marathon2 ~ trend)
fcasts.lin <- forecast(fit.lin, h = 10)

# exponential trend
fit.exp <- tslm(marathon2 ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = 10)

# piecewise linear trend
t.break1 <- 1980
t <- time(marathon2)
t1 <- ts(pmax(0, t-t.break1), start = 1940)
fit.pw <- tslm(marathon2 ~ t + t1)
t.new <- t[length(t)] + seq(10)
t1.new <- t1[length(t1)] + seq(10)
newdata <- data.frame("t" = t.new, "t1" = t1.new)
fcasts.pw <- forecast(fit.pw, newdata = newdata)


## ---- echo=FALSE, fig.height=4-------------------------------------------------
autoplot(marathon2) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fcasts.pw, series = "Piecewise") +
  autolayer(fcasts.lin$mean, series = "Linear") +
  autolayer(fcasts.exp$mean, series = "Exponential") +
  xlab("Year") +  ylab("Winning times in minutes") +
  guides(colour = guide_legend(title = " "))


## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------------
fit.lin$method <- "linear trend regression model"
checkresiduals(fit.lin, test = FALSE)


## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------------
fit.exp$method <- "exponential trend regression model"
checkresiduals(fit.exp, test = FALSE)


## ----residPiecewise, message=FALSE, warning=FALSE, echo=FALSE------------------
fit.pw$method <- "piecewise linear regression model"
checkresiduals(fit.pw, test = FALSE)


## ---- echo=FALSE, fig.height=3.5-----------------------------------------------
# read quarterly data
data <- read.csv("data/covid19.csv", header = TRUE)
# get time series
ab.conf <- data[data$prname=="Alberta", c("date","numconf")]
t.start <- which(ab.conf$date=="15-03-2020")
t.end <- nrow(ab.conf)
ab.conf.ts <- ts(ab.conf$numconf[t.start:t.end])
# plot
autoplot(ab.conf.ts) + xlab("Year") + ylab(" ") + 
  ggtitle("Total cases of COVID-19 in Alberta since March 15, 2020")


## ---- echo=FALSE, fig.height=3.5-----------------------------------------------
dab.conf.ts <- diff(ab.conf.ts)
autoplot(dab.conf.ts) + xlab("Year") + ylab(" ") + 
  ggtitle("New cases of COVID-19 in Alberta since March 15, 2020")


## ----beeryetagain2, echo=FALSE-------------------------------------------------
#  forecasts of beer production using regression
autoplot(fcast) + xlab("Year") + ylab("megalitres")


## ----usconsumptionf, echo=TRUE-------------------------------------------------
fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment, data = uschange)
h <- 4
# increase
newdata <- data.frame(
    Income = c(1, 1, 1, 1),
    Savings = c(0.5, 0.5, 0.5, 0.5),
    Unemployment = c(0, 0, 0, 0)
)
fcast.up <- forecast(fit.consBest, newdata = newdata)
# decrease
newdata <- data.frame(
    Income = rep(-1, h),
    Savings = rep(-0.5, h),
    Unemployment = rep(0, h)
)
fcast.down <- forecast(fit.consBest, newdata = newdata)


## ----usconsumptionf2, echo=FALSE-----------------------------------------------
autoplot(uschange[, 1]) + ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

