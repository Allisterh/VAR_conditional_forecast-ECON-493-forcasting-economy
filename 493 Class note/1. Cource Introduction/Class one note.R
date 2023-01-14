install.packages("tidyverse")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("lubridate")  
install.packages("mFilter")
install.packages("neverhpfilter")
install.packages("tsbox")

library(tidyverse)
library(lmtest)
library(ggplot2)
library(lubridate)  
library (mFilter)      # HP Filter
library (neverhpfilter)# Hamilton Filter
library (ggplot2)      # For Graphs
library (tsbox)        # Convert xts to ts


#make sure clean the environment
rm(list = ls())
dev.off()



# read data
DATA <- read.csv("data/cyclistsTempHKI.csv", header = TRUE)
head(DATA, 100)
#read. csv() function reads a file into data frame. CSV file can be comma delimited or tab or any other 
#delimiter specified by parameter "sep=". If the parameter "header=" is "TRUE", then the first row will be treated as the row names.


#Transfer data into times serise verson



ggplot(DATA, aes(meanTemp, cyclists)) + #you can using the variable name rather than the data_set$variable
  geom_point() + #散点图
  geom_smooth(span = 0.3) 
  # Span: control the amount of smoothing for the defalut loess smoother.
  #smaller the number produce wigglier lines, larger  numbers produce smoother lines. 



Time.ts <- ts(DATA$date)
Average_Tempure.st <- ts(DATA$meanTemp)
Lowest_tempure.st <- ts(DATA$minimumTemp)
Snow_depth.st <- ts(DATA$snowDepth.cm.)



autoplot(DATA)
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts)
ggseasonplot(a10, polar=TRUE)
