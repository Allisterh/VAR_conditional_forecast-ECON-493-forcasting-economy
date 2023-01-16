library("ggplot2")
library("fpp2")

# The linear regression model 
  #The linear regression
  #the least square 
  #the R^2 of the regression, its between the zero and 1, or it can be the negative if there is not intercation, it 
    #how much is the movment cin te hy can be explain by the x,  the regreion mode is iexplain the relation bryween teh y
    #with x (its just the sameas I learned in the 399, thanks god my professor is gooood!)

#should we talke care of the r^2? (goodness of fit)
  #no, because it is just something how the model can explain the y and x. 
  #we do nto know the correct model? or reasonable? 

#the standard error of the regesion is another summary of hte model
  #it can be calculated as follows 
  # k is the number of preditor in the modle 

#for forcasting pruposed, we reuqir the following assu,ption
   #the erro are uncorrlated and zero mean 
    #the error and uncorrelated with eah 

#its useful t. also have the normally disction, otherwise, you manybe be need tobe careful on how to deal twith model.

#other things to watch
  #teh outlinear
  #influential obse3rcations
  #data should not be removed without a good explanatation of why rey are different.

#to compete apredition interva l
  #ingnoring parater estimation uncertainty
  #and assuming forecasti errors are norally distributed. 
  #the prediction internval
  #hwhen theinterval is too big, Th. model becase uneless

#lm(): linear regression model
#tslm(): regression model for time series data
#summary(): prints standard regression output
#coef(), vcov(), resid(), fitted(): extract the regression
#coe cients, (estimated) covariance matrix, residuals, and  tted
#values respectively
#confint(): con dence intervals for the regression coe cien
#predict(): predictions for new data
#coeftest: coe cient tests
#NeweyWest(): Newey-West HAC covariance matrix
#vcovHAC(): more HAC covariance matrices

# simulate data
n.obs <- 200
x1 <- rnorm(n.obs)
x2 <- rnorm(n.obs)
x3 <- rnorm(n.obs)
y <- .75*x1 + .50*x2 + .25*x3 + rnorm(n.obs, mean = 0, sd = 2)
# some irrelevant variables
x4 <- rnorm(n.obs, mean = 0, sd = 4)
x5 <- rnorm(n.obs, mean = 0, sd = 5)
# set data frame
data <- data.frame(y, x1, x2, x3, x4, x5)
head(data, 2)

#Consider the following three models to be estimated:
#yi on xi1
#yi on xi1, xi2, xi3
#yi on all  ve variables

model1 <- lm(y ~ x1) # missing variables
model2 <- lm(y ~ x1 + x2 + x3) # correctly specified model
model3 <- lm(y ~ x1 + x2 + x3 + x4 + x5) # irrelevant variables

coefficients (model2)
coefficients (model1)
coefficients (model3)

#for the data that are coeeffiiicne 

# add new observation
new <- list(x1 = 1, x2 = 1, x3 = 1, x4 = 1, x5 = 1)
# predict y using model 2
pred_new <- predict( #get the data 
  model2,
  newdata = new, #create the new data =
  se.fit = TRUE, #what is this ?
  interval = "prediction"
)
pred_new$fit

#wher there are manyt predictors, how should we chosiise which one to use. 

# things tyou should not do 
  #plot y againesta particular predictor(x) and it it shows no noticeablerelationship, drop it
  #do multiplue leaner regression on tall the predictor and disregrade all variable whose p value are greater than 0.01
  #maximize r^2 on minize mse 
   #the r^2 will risng when the more variable add.

# computre outpure for regression will alwa giove the r^@
  #the r^2 and adjust r^@
  
#the bias cariancetrade off 
   #ware we usew more flexible morl pr the vairance will increse and the biasd will decresea.

# the model contraining all the preidctor will always have the smallest ssr and the leagest r^2
  # sltuion: intouion the trade off 

#the trw different thisn 
  #very close relativethe the r trade off, a bitger model will be better fit, becasue you are fitted the data correction but cere is 
  #hasthe punishmnet, yyo hs th. smaily logive as the R^2. 

#vic andaic can be usd whe nteh model are note. netst 
#model with loest vic and acic are prefreerd
#bic has beavier penaity (slects msmaller model )

#unlied the t^2 astatics, the adjustmened r^2 penalized the includion of the unnecessary variabes in the model.


CV(model1)
CV(model2)
CV(model3)
#note, the bic penity punishment heavy

#in same model, then we look for some static, we choice you think how wee we feed the dat, but its dhow confliction model and. model [eople howp eople working 
#that is not the way we usually want to use hte mdoe , 
# the challgne of model of atat htat will becasme the fulture
# the corrose validation, we will using the different data tofit htedata to check it! 
  #the same data (just like the using hte old the data to frocasnt ghe data we hve right neow)
  #it is useufl for find the unless the varialbe for the model, that mean this new mentaion is better.

#Mean suqred error
   #in the regression setting we can use the means equared erorr 
  # this is the most the useful,

#theeree iotuons 
  #the valdation set approach 
 # leave one out cress valiadation 
 # k-fokd creoss validation


