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



## ---- echo=FALSE---------------------------------------------------------------
data <- read.csv("data/uruguay.csv", header = TRUE)
uru <- ts(data$Value, start = c(2004,1), freq = 12)
autoplot(uru) + ylab(" ") + 
  ggtitle("Google Trends (Canada): 'Uruguay'")


## ----tstable, echo=FALSE, cache=TRUE-------------------------------------------
x <- c(123,39,78,52)
yr <- 2012:2015
knitr::kable(data.frame(Year=yr,Observation=x), booktabs=TRUE)


## ---- echo=TRUE----------------------------------------------------------------
# read canada quarterly real gdp data
data <- read.csv("data/NAEXKP01CAQ661S.csv", header = TRUE)
cangdp <- ts(data$NAEXKP01CAQ661S, start = 1961, freq = 4)
# display some observations
window(cangdp, start = c(2001,1), end = c(2005,4))


## ---- echo=TRUE, fig.height=3.5------------------------------------------------
# time series plot
autoplot(cangdp) + xlab("Year") + ylab(" ")


## ---- echo=FALSE---------------------------------------------------------------
# economy class passengers
autoplot(melsyd[,"Economy.Class"]) + 
  ylab("thousands") + xlab("Year") + 
  ggtitle("Economy class passengers: Melbourne-Sydney")


## ----a10, echo=TRUE------------------------------------------------------------
# drug sales
autoplot(a10) + ylab("$ million") + xlab("Year") +
  ggtitle("Antidiabetic drug sales")


## ---- echo=FALSE---------------------------------------------------------------
data <- read.csv("data/googletrends_applepie_ca.csv", header = FALSE)
canpie <- ts(data$V2, start = c(2013,33), freq = 52.18)
autoplot(canpie) + ylab(" ") + 
  ggtitle("Google Trends: 'Apple Pie Recipe' in Canada")


## ---- echo=FALSE---------------------------------------------------------------
data <- read.csv("data/googletrends_applepie_us.csv", header = FALSE)
uspie <- ts(data$V2, start = c(2013,33), freq = 52.18)
autoplot(uspie) + ylab(" ") + 
  ggtitle("Google Trends: 'Apple Pie Recipe' in US")


## ---- echo=FALSE---------------------------------------------------------------
# drug sales
autoplot(a10) + ylab("$ million") + xlab("Year") +
  ggtitle("Antidiabetic drug sales")


## ---- echo=TRUE----------------------------------------------------------------
# seasonal plot
ggsubseriesplot(a10) + ylab("$ million") +
  ggtitle("Subseries plot: antidiabetic drug sales")


## ---- echo=FALSE---------------------------------------------------------------
autoplot(window(elec, start=1980)) + xlab("Year") + ylab("GWh") +
  ggtitle("Australian electricity production")


## ---- echo=FALSE---------------------------------------------------------------
autoplot(hsales) + xlab("Year") + ylab("Total sales") +
  ggtitle("Sales of new one-family houses, USA") 


## ---- echo=FALSE---------------------------------------------------------------
autoplot(ustreas) + xlab("Day") + ylab("price") +
  ggtitle("US Treasury Bill Contracts")


## ------------------------------------------------------------------------------
# plor beer data
autoplot(beer)


## ----beeracf-------------------------------------------------------------------
# plot autocorrelations
ggAcf(beer) + ggtitle("")


## ------------------------------------------------------------------------------
# electricity data
elec2 <- window(elec, start = 1980)
autoplot(elec2)


## ------------------------------------------------------------------------------
# plot autocorrelations
ggAcf(elec2, lag.max = 48) + ggtitle("")


## ------------------------------------------------------------------------------
# plot data
autoplot(goog)


## ------------------------------------------------------------------------------
# plot autocorrelations
ggAcf(goog, lag.max = 100) + ggtitle("")


## ---- echo=TRUE----------------------------------------------------------------
# simulate white noise process
wn <- ts(rnorm(36))
autoplot(wn)


## ----wnacf---------------------------------------------------------------------
# plot autocorrelations
ggAcf(wn) + ggtitle("")


## ---- echo=FALSE, fig.height=2.5-----------------------------------------------
ggAcf(wn) + ggtitle("")


## ---- echo=FALSE---------------------------------------------------------------
# read data
data <- read.csv("data/covid19.csv", header = TRUE)
# get time series
ab.conf <- data[data$prname=="Alberta", c("date","numconf")]
t.start <- which(ab.conf$date=="15-03-2020")
t.end <- nrow(ab.conf)
ab.conf.ts <- ts(ab.conf$numconf[t.start:t.end])
# plot
autoplot(ab.conf.ts) + xlab("Year") + ylab(" ") + 
  ggtitle("Total cases of COVID-19 in Alberta since March 15, 2020")


## ---- echo=FALSE---------------------------------------------------------------
ggAcf(ab.conf.ts) + 
  ggtitle("ACF total cases")


## ---- echo=FALSE---------------------------------------------------------------
# diff data
dab.conf.ts <- diff(ab.conf.ts)
autoplot(dab.conf.ts) + xlab("Year") + ylab(" ") + 
  ggtitle("New cases of COVID-19 in Alberta since March 15, 2020")


## ---- echo=FALSE---------------------------------------------------------------
ggAcf(dab.conf.ts)  + 
  ggtitle("ACF new cases")

