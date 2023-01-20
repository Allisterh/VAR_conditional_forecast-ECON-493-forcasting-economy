library("ggplot2")
library("fpp2")

# The chapter 4: the shrinkage methods

#Suppose we observe a dependent variable y and p potential explanatory variable. 
#Two main resons to perform variable selection
  #model interpertability: including irrelevant variables leads to unecessary complexity
  #prediction accouracy: including irrelevant variables leads to less accurarte predictions,
    #specially when N is not much larger than the p

#The shrinkage methods 
   #Shrinkage methods involve tting a model with all p predictors (even if p > N).
    #estimated coe cients are shrunk towards 0, some may be estimated to be exactly 0
    #shrinkage reduces the variance of the estimators

