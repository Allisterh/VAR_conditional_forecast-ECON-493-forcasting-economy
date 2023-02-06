#i love code, code make me hapy 

#some of the package may not been used in this homework
library("ggplot2")
library("fpp2")
library("glmnet")
library("tidyr")
library("lmtest")
library("boot")


#The linear model with series data

#The residual diagnostics 
  #for the forecasinty purposes, we require the following assumptions 
    #the ridusal are uncorrelated and zero. mean
    #residuenal are uncorrelated with each other
# it issueful to asl o have the stand. devidation of not
#inversal or doing statical tests.


#The breusch_godfrey test
  #the Breusch-Godfrey test is better than Ljung-Box for regression models

fit.consMR <- tslm( Consumption ~ Income + Production + Unemployment + Savings, data = uschange ) 
coeftest(fit.consMR)
checkresiduals(fit.consMR, test = FALSE)

bgtest(fit.consMR)
#Breusch-Godfrey test for serial correlation of order up to 1
#data:  fit.consMR
#LM test = 1.4543, df = 1, p-value = 0.2278


# if the model fial the bg test
  #theforecasts are not worong, but have higher variance than they need to 
  #there is information in the residuals that we should exploit


#trend
#the seaosal dummy varialvbne

#beware of the dummy variable trap
  #using one dummy for each category gives too many dummy variables!
  #the regression will then be singular and inestimable
  #either omit the constant, or omit the dummy for one category
  #the coeﬃcients of the dummies are relative to the omitted category


#other useful predictors  
  #spikes: variable equals 1 at the intervention and 0 elsewhere
    #useful to remove the effect of an outlier
  #steps: variable equals 0 before the intervention and 1
    #afterward (useufl for the mdel structure breaks)
  #change of slop valriable equals 0 before intervention 


#For monthly data ...
#Christmas: always in December so part of monthly seasonal eﬀect 
#Easter: use a dummy variable v t = 1 if any part of Easter is in that month,
#v t = 0 otherwise 
#Ramadan and Chinese new year similar


#To compute a prediction interval ...
  #ignoring parameter estimation uncertainty (that is, sampling ˆ error in y 0 )
  #and assuming forecast errors are normally distributed, then an approximate 95% PI is










