495. 
Tie.Ma. 
1537905. 

#Step one Lode the all package that necessary. 
library (lubridate)    
library (cansim)       
library (OECD)        
library (WDI)          
library (fredr)        
library (tsbox)
library (RColorBrewer)
library(wesanderson)
library(writexl)
library(readxl)
library(ggplot2)
```
 
This following code include the data collection for the 495 economic forecast.
1. Consumer Price Index, monthly, not seasonally adjusted
(https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1810000401)
2. new housing price (note:stat say it is level E data)
https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1810020501
v111955443





CPI_shelter_price_raw<- "v41691050"
  #set the vector name because number is confused
CPI_shelter_price.st<- get_cansim_vector(CPI_shelter_price_raw, start_time = "2002-01-01")
  #get the data from the stat canada
CPI_shelter_price_year.st <- year(CPI_shelter_price.st$REF_DATE[1])
  #take year variable out
CPI_shelter_price_month.st <- month(CPI_shelter_price.st$REF_DATE[1])
  #take month variable out
  c(CPI_shelter_price_month.st, CPI_shelter_price_year.st)
  
CPI_shelter_price.ts<- ts(CPI_shelter_price.st$VALUE, start = c(CPI_shelter_price_month.st, CPI_shelter_price_year.st), freq = 12)
autoplot(CPI_shelter_price.ts)



CPI_shelter_price.ts<- ts(CPI_shelter_price.st$VALUE, start = c(CPI_shelter_price_month.st, CPI_shelter_price_year.st), freq = 12)
autoplot(CPI_shelter_price.ts)


#It look bad need log transformation
CPI_shelter_price_growth_rate <- diff(CPI_shelter_price.ts) / stats::lag(CPI_shelter_price.ts, 1)
autoplot(CPI_shelter_price_growth_rate)



2. the new housing price index
housing_price_index_raw<- "v111955443"
  #set the vector name because number is confused
housing_price_index.st<- get_cansim_vector(housing_price_index_raw, start_time = "2002-01-01")
  #get the data from the stat canada
housing_price_index_year.ts <- year(housing_price_index.st$REF_DATE[1])
  #take year variable out
housing_price_index_month.st <- month(housing_price_index.st$REF_DATE[1])
  #take month variable out
  c(housing_price_index_year.ts, housing_price_index_month.st)
  
housing_price_index.ts<- ts(housing_price_index.st$VALUE, start = c(housing_price_index_year.ts, housing_price_index_month.st), freq = 12)
autoplot(housing_price_index.ts)


#It look bad need log transformation
housing_price_index_growth_rate <- diff(housing_price_index.ts) / stats::lag(housing_price_index.ts, 1)
autoplot(CPI_shelter_price_growth_rate)

#create a file that I will put my excel
dir.create("excel")

#import excel data

The_housing_sell_data <- read_excel("excel/The housing sell data.xlsx", col_types = c("date", "numeric", "numeric","skip", "skip"))


The_housing_sell_data.raw <- as.data.frame(The_housing_sell_data)
head(The_housing_sell_data.raw, 5)

df_melted <- melt(The_housing_sell_data.raw, id.vars = "data", variable.name = "group")




ggplot(The_housing_sell_data.raw, aes(x = Date)) + 
  geom_line(aes(y = Canada, color = "Canada")) + 
  geom_line(aes(y = `10-year average`, color = "10-year average")) + 
  labs(title = "Housing Sell Data", x = "Date", y = "Data")

noel 





The_housing_sell_data.ts <- ts(The_housing_sell_data_raw, frequency = 12)

head(The_housing_sell_data.ts)
plot(The_housing_sell_data_raw)

                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               )
