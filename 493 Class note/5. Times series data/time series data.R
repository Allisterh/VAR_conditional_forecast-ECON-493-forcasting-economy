#Step one Lode the all package that necessary. 

library ("lubridate")    
library ("cansim")       
library ("OECD")        
library ("WDI")          
library ("fredr")        
library ("tsbox")
library ("RColorBrewer")
library("wesanderson")
library("writexl")
library("ggplot2")
library("fpp2")
library("glmnet")
library("mosaicCalc")
library("tidyr")
library("lmtest")
library("boot")

###########
#you build the model and the model wullhave some redisula diagnostics 
  #the residuals should be unrelated.(missing of the varialbe)
  #the residuals should be mean zero ( you miss the intercept)



