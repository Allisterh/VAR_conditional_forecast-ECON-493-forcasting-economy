
install.packages("r2symbols")
install.packages("ggplot2")
install.packages("fpp2")
install.packages("glmnet")
install.packages("tidyr")
install.packages("lmtest")
#library("r2symbols") #各种奇形怪状的符号
library("ggplot2")
library("fpp2")
library("glmnet")
library("mosaicCalc")#微积分计算器！
library("tidyr")
library("lmtest")
library("boot")





#ridge regress vs the lasso
#we are left ith some example 
#Ridge regression vs. lasso
#so, just eh reference, the ridge regession and lass ceficients solve the problems.
#but the key thigns things here is we include the panoty
# the regess is different
# the beta hear is the unrearisced ols soltiuon, the lasso and ridg regress solution solution may differ. 

#this day, we have the big data, the most of data is most unsless, the most challange is the how to find
  #the useful data, speicfic when you haver over 100000 variance, therefore the lasso is gooood, It mean th 
  #lasso is useful for the reduce the variable size. 


#which is btter?
  #the laasso should erform better when a relatively small number of predictors are 
  #improtance and the remianing predictor have coefiicients that are very small 

#Which is better?
  
 # the lasso should perform better when a relatively small number of predictors
#are important and the remaining predictors have coeﬃcients that are very small 
#or equal to 0 ridge regression should perform better when many predictors are (roughly) 
#equally important cross-validation can be used to determine which approach is 
#better in a particular data set


# simulate data
n.obs <- 200 
n.var <- 50 # 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors
x <- matrix(0, n.obs, n.var) 
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) } 
y <- .75*x[,1] + .50*x[,2] + e 
x <- scale(x) #make the sample equal to the excist 0
data.all <- data.frame(x,y)


grid <- 10^seq(3, -3, length = 100)

#ridge reg for different values of lambda 

ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

#lasso reg for different values of lambda 

lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)


plot(ridge.mod, xvar = "lambda", label = TRUE)


#######################

# split the sample into train/test sets 
train <- sample(nrow(data.all), round(nrow(data.all)/2)) cv.error <- rep(NA,3)

# least squares 
ols.cv <- lm(y ~ x, data = data.all, subset = train) cv.error[1] <- mean((y - predict(ols.cv, data.all))[-train]ˆ2)

# ridge

ridge.cv <- cv.glmnet(x[train,], y[train], alpha = 0, nfolds = 10)

ridge.lam <- ridge.cv$lambda.min cv.error[2] <- mean((y - predict(ridge.cv, s = ridge.lam, newx = x))[-train]ˆ2)

# lasso

lasso.cv <- cv.glmnet(x[train,], y[train], alpha = 1, nfolds = 10)

lasso.lam <- lasso.cv$lambda.min cv.error[3] <- mean((y - predict(lasso.cv, s = lasso.lam, newx = x))[-train]ˆ2)

# remakrs 
  #oLS PERFROMS PORRLY (relative to the other mehtods)
  #Ridge and lasso work best in this data set 



# simulate data 
n.obs <- 2000
n.var <- 5000 # 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors
x <- matrix(0, n.obs, n.var) 
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) } #
y <- .75*x[,1] + .50*x[,2] + e 
x <- scale(x)
data.all <- data.frame(x,y)
# lasso reg for different values of lambda 
  lasso.mod <- glmnet(x, y, alpha = 1, lambda = 10^seq (3, -3, length = 100))
  plot (lasso.mod, xvar = "lambda", label = TRUE)
  
  
  lasso.cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10) 
  plot(lasso.cv)
  
  
  
  coef.ols <- rep(NA, n.var)
  test.ols <- rep(NA, n.var)
  
  for (i in 1:n.var){
    
    tmp0 <- lm(y ~ x[,i])
    
    coef.ols[i] <- coeftest(tmp0)[2,1]# estimated slope
    
    test.ols[i] <- abs(coeftest(tmp0)[2,3]) # t-stat
    
  } # single regression coefficients 
  
  coef.ols <- data.frame(
    
    "x" = seq(1:n.var),
    
    "beta" = coef.ols,
    
    "test" = test.ols)
  
  plot(coef.ols)
  
  
  #for the lasso is popular, do not dod hte thigns do ing the hundard of reression 
  # something interest may rising eher
  # any question? 
  
  
  
  