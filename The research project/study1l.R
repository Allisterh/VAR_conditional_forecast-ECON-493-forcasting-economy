#######################################
# ECON 485
# Conditional VAR Forecasting using 1-step Ahead Forecast Iteration

# Initialize
rm(list = ls())
#dev.off(dev.list()["RStudioGD"])

#setwd("C:/Users/James/My Drive/R/gov_chall/The-Governor-s-Challenge")
setwd("G:/My Drive/University/Econ 485/R")

# Packages
library(cansim)       # Get data from StatsCan
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(lubridate)    # Easy date conversions
library(openxlsx)
library(gdata)
library(stringr)
library(xts)
library(tsbox)

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")
source("functions/forecast_conditional_var.R")

######################################
## Parameters

date.start <- '1993-01-01'


###################################### 

## 1. Data ----


# 1.1 Reading in Data
# You should document where your data comes from (not like me here!)
cpi <- ts_cansim("v41690914", start = date.start)
gdp <- ts_cansim("v65201210", start = date.start)
gdp.old <- ts_cansim("v41881478", start = date.start)  # GDP with older base year (terminated)
ten.year <- ts_cansim("v122543", start = date.start)
wage <- ts_cansim("v103451670", start = date.start)
target.d <- ts_cansim("v39079", start = date.start)    # daily target rate 
u <- ts_cansim("v2062815", start = date.start)

fredr_set_key('7b15ffc9ff456a8d5e3e579d2b04a9f8')
wti <- ts_fred('MCOILWTICO', start = date.start) # WTI oil price

# Get Monthly US GDP
## Monthly US GDP estimates from: https://ihsmarkit.com/products/us-monthly-gdp-index.html
url.usgdp <- "https://cdn.ihsmarkit.com/www/default/1020/US-Monthly-GDP-History-Data.xlsx"
df.usgdp <- read.xlsx(url.usgdp, sheet = 'Data')
gdp.us <- ts(df.usgdp$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)

# Get Monthly US GDP
## Monthly US GDP estimates from: https://ihsmarkit.com/products/us-monthly-gdp-index.html
url.gscpi <- "https://www.newyorkfed.org/medialibrary/research/interactives/gscpi/downloads/gscpi_data.xlsx"
df.gscpi <- read.xls(url.gscpi, sheet = 'GSCPI Monthly Data', blank.lines.skip=TRUE)
gscpi <- ts(df.gscpi$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)

# 1.2 Variable Transformations

## a) Converting daily target rate to monthly
# convert to xts object
seq.time <- seq(as.Date(date.start), by = 'day', length.out =  length(target.d))
target.xts.d <- xts(as.numeric(target.d), seq.time)
# create the most frequent value aggregation function
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
# aggregate
target.xts <- aggregate(target.xts.d, as.yearmon, calculate_mode)
#target.xts <- apply.monthly(target.xts.d, calculate_mode) # Leave as rate
target <- as.ts(target.xts)

## b) Splicing Monthly GDP data together
plot(cbind(gdp,gdp.old))
# regress gdp.old on gdp.new
ind.overlap <- complete.cases(cbind(gdp, gdp.old))
ind.predict <- is.na(cbind(gdp, gdp.old)[,'gdp'])
df.overlap <- as.data.frame(cbind(gdp, gdp.old)[ind.overlap,])
df.predict <- as.data.frame(cbind(gdp, gdp.old)[ind.predict,])
mod.splice <- lm(gdp ~ gdp.old, data = df.overlap)
# predict new values for old dates
gdp.hat <- predict(mod.splice, newdata = df.predict)
gdp.splice <- ts(c(gdp.hat, gdp), start = c(year(date.start),month(date.start)), freq = 12)
# check
plot(gdp.splice)
lines(gdp, col = 'red')
# looks good

## c) Making Series Stationary
GDP <- diff(log(gdp.splice), 12)       # Convert to yoy GDP growth
INF <- diff(log(cpi), 12)       # Convert to yoy inflation
U <- u                          # Leave as rate
TARGET <- target                # Leave as rate
GDP.US <- diff(log(gdp.us), 12) # Convert to yoy GDP growth
WTI <- wti                      # Leave as price level
