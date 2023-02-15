library("ggplot2")
library("fpp2")
library("glmnet")
library("mosaicCalc")#微积分计算器！
library("tidyr")
library("lmtest")
library("boot")
####################

#homework_one

###################
#################################################################################################################
#Exercise 4 
########################################################################################################################################
#generate the simulated data

#################################################################################################################
set.seed(1234)
n.obs <- 100
x1 <- rnorm(n.obs)
y <- x1 - 2*x1^2 + rnorm(n.obs)
#################################################################################################################
#4-b
df <-data.frame(y,x1)
plot(df, pch=1,col='#003b6f', type = "p")


#save the graphy
pdf("pics/TieMa_homework1_Q4_b.pdf", height = 9, width = 12)
plot(df, pch=1,col='#003b6f', type = "p")
dev.off()

#clean the enviroment!
rm(list = ls())
#4-b-2
#From the graphy we could find nonlinear relationship between x1 and y
#################################################################################################################
#Q4-c

######################################################
#generate the simulated data (again, in order to avoid this section been polluted)
set.seed(1234)
n.obs <- 100
x1 <- rnorm(n.obs)
x2 <- x1^2
x3 <- x1^3
x4 <- x1^4
y <- x1 - 2*x1^2 + rnorm(n.obs)
Q4_data_set <- data.frame(y, x1, x2, x3, x4)
######################################################

model_one <- lm(y ~ x1, data=Q4_data_set)
##########

model_two <- lm(y ~ x1+x2, data=Q4_data_set)
#########

model_three <- lm(y ~ x1+x2+x3, data=Q4_data_set)
##########

model_four <- lm(y ~ x1+x2+x3+x4, data=Q4_data_set)

AIC(model_one)
BIC(model_one)
extractAIC(model_one)
CV(model_one)
BIC(model_one)

evil <- rbind(CV(model_one), CV(model_two), CV(model_three), CV(model_four))
evil
rownames(evil) <- c('Model1', 'Model2', 'Model3', 'Model4')
evil

#.          CV       AIC      AICc       BIC       AdjR2
#Model1 9.217431 218.96020 219.21020 226.77571 0.009513121
#Model2 1.094918  11.79099  12.21204  22.21167 0.876435778
#Model3 1.101478  13.45234  14.09064  26.47819 0.875570749
#Model4 1.115254  15.42932  16.33255  31.06034 0.874289903


#by compare AIC and BIC we can concluded following
#model 2 > model 3 > model4 > model 1
#the smaller the betterm therefore, the model 2 have lowest AIC and BIC, it is relative better model to use 

#################################################################################################################
# Q4 - v Compute the k-fold cross-validation errors that result from fitting the four models. Use
#k = 5. Which model would you prefer? Is this what you expected? Explain your answer.

#################################################################################################################################
#clean the enviroment and generate everything again....
rm(list = ls())

#create the chart that carry the number od the cross-vaidation error
cv.error <- rep(NA,4)

#simulate data and enviorment.

set.seed(1234)
n.obs <- 100
x1 <- rnorm(n.obs)
x2 <- x1^2
x3 <- x1^3
x4 <- x1^4
y <- x1 - 2*x1^2 + rnorm(n.obs)
Q4_data_set_2 <- data.frame(y, x1, x2, x3, x4)

head(Q4_data_set_2, 2) #check it!!


model_one_with_data_set.cv <- glm(y ~ x1)
cv.error[1] <-cv.glm(Q4_data_set_2, model_one_with_data_set.cv, K=5)$delta[1]
print(cv.error)


#################################################################################################################################
#model 2 data set

model_two_with_data_set.cv <- glm(y ~ x1 + x2)
cv.error[2] <-cv.glm(Q4_data_set_2, model_two_with_data_set.cv, K=5)$delta[1]
print(cv.error)

#################################################################################################################################
#model 3

model_three_with_data_set.cv <- glm(y ~ x1 + x2 + x3)
cv.error[3] <-cv.glm(Q4_data_set_2, model_three_with_data_set.cv, K=5)$delta[1]
print(cv.error)
##################################################################################################################################
#model 4


model_four_with_data_set.cv <- glm(y ~ x1 + x2 +x3 +x4)
cv.error[4] <-cv.glm(Q4_data_set_2, model_four_with_data_set.cv, K=5)$delta[1]
Q <-cv.glm(Q4_data_set_2, model_four_with_data_set.cv, K=5)$delta[1]
print(cv.error)
#I spend 3 hours on those code, god its finally down.....
##################################################################################################################################

# The 5- fold corss-validation errors
#8.865048 1.078610 1.157496 1.115773

#low fold cross validation erros may relate to the problem of over fitting, 
#the AIC and BIC test are working on find the model that fit the data better with relative smllar variance
# It show the similar perference as the result with. the AIC and BIC test. 
##################################################################################################################################


model_one <- lm(y ~ x1, data=Q4_data_set)
##########

model_two <- lm(y ~ x1+x2, data=Q4_data_set)
#########

model_three <- lm(y ~ x1+x2+x3, data=Q4_data_set)
##########

model_four <- lm(y ~ x1+x2+x3+x4, data=Q4_data_set)

summary(model_one)
#Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -2.0980     0.2966  -7.074 2.25e-10 ***
#  x1            0.4095     0.2932   1.397    0.166  

summary(model_two)
#.           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.13954    0.13506   1.033    0.304    
#x1           1.00098    0.10597   9.446 2.11e-15 ***
#x2          -2.09591    0.07987 -26.241  < 2e-16 ***


summary(model_three)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.13247    0.13610   0.973    0.333    
#x1           0.91259    0.18789   4.857 4.63e-06 ***
#x2          -2.10637    0.08222 -25.618  < 2e-16 ***
# x3           0.03231    0.05661   0.571    0.570 

summary(model_four)
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.118715   0.165425   0.718    0.475    
#x1           0.910799   0.189241   4.813 5.60e-06 ***
#x2          -2.075771   0.222837  -9.315 4.81e-15 ***
#x3           0.034229   0.058369   0.586    0.559    
#x4          -0.006444   0.043575  -0.148    0.883    


##################################################################################################################################

#clean the enviroment!
rm(list = ls())

##################################################################################################################################
#Exercise 5 
##################################################################################################################################
#Exercise 5-a
#Create a matrix X (545 × 9) with the 7 explanatory variables described above plus experience and schooling squared. 
#Scale the matrix X such that all variables have the same variance. Create a vector y (545 × 1) with log wage.
library("leaps")

#lode the data
X_df <-read.csv("data/males1987.csv", header = TRUE)

#check the data!
class(X_df)
str(X_df)
head(X_df, 4)

#move the logwage in the first col
X_relocated <- relocate(X_df, LOGWAGE, before = )
#AFTER SPEND 3 HOURS FINALLY DOWN

#check the data!
str(X_relocated)
head(X_relocated, 4)
#It look good


#now square experience and school

X_relocated_final<- X_relocated %>% select(LOGWAGE, BLACK, EXPER, HISP, MAR, SCHOOL, UNION, EXPER2) %>% mutate(SCHOOL2 = SCHOOL^2)

#check the data
head(X_relocated_final, 4)
#its look good!

X_Scale <- X_relocated_final%>% 
  select(LOGWAGE, BLACK, EXPER, HISP, MAR, SCHOOL, UNION, SCHOOL2, EXPER2)  %>% 
  transmute(LOGWAGE_scale =scale(LOGWAGE) , BLACK, EXPER_scale = scale(EXPER), 
            HISP, MAR, SCHOOL_scale = scale(SCHOOL), UNION, EXPER2_scale = scale(EXPER2), SCHOOL2_scale = scale(SCHOOL2))
#sorry for this line of code is way too long...
#I did not really sure how to shrink it down...
#It select the all the variable 
#and using the scale function to scale the non-dummy variable...


#check the data!
head(X_Scale, 4)
#it look good!

#check what the data is
class(X_Scale)
# its data frame! 

#transfer to matrix
X <- data.matrix(X_Scale)

#check if the data actually been scale()
#summary(X_Scale)
#var.all <- rep(NA,5)
#var.all[1] <- var(X_Scale$LOGWAGE_scale)
#var.all[2]<- var(X_Scale$EXPER_scale)
#var.all[3]<- var(X_Scale$SCHOOL_scale)
#var.all[4]<- var(X_Scale$EXPER2_scale)
#var.all[5]<- var(X_Scale$SCHOOL2_scale)
#print(var.all)

#[1] 1 1 1 1 1
#var(X_Scale$SCHOOL2_scale)
#all non dummy variable have the same variance 

#Create a vector y with log wage
#recall we have the matrix X that include all the data

x<- X_Scale%>%
  dplyr::select(
    BLACK, EXPER_scale, HISP, MAR, SCHOOL_scale, UNION, EXPER2_scale,
    SCHOOL2_scale
  )%>%
  data.matrix()

#check the data!
head(x,5)

y_THE_LOGWAGE <- X_Scale$LOGWAGE_scale
data_question5 <- data.frame(y_THE_LOGWAGE, x)

#check the data
head(data_question5, 5)

#the final matrix
matrix_X <- data.matrix(data_question5)

##################################################################################################################################
#Q5-b
# ridge regression for different values of lambda
evil_grid <- 10^seq(3, -3, length = 10000)
  #set the lambta from 10 ^3 to 10^3 
ridge.mod <- glmnet(x, y_THE_LOGWAGE, alpha = 0, lambda = evil_grid)
  #alpha = 0 is ridge
  #alpha = 1 is lasso
  
ridge.mod1 <- cv.glmnet(x, y_THE_LOGWAGE, alpha = 0, lambda = evil_grid)

#plot ridge results
plot(ridge.mod, xvar = "lambda", label = TRUE, main = "ridge regression standardized coeﬃcients")
plot(ridge.mod1)
print(ridge.mod1)

#save the graphy
pdf("pics/TieMa_homework1_Q5_b_ridge.pdf", height = 9, width = 12)
plot(ridge.mod, xvar = "lambda", label = TRUE, main = "ridge regression")
dev.off()

#plot the lasso
evil_grid <- 10^seq(3, -3, length = 100)
lasso.mod <- glmnet(x, y_THE_LOGWAGE, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE, main = "lasso regression")
print(lasso.mod)
csa <- cv.glmnet(x, y_THE_LOGWAGE, alpha = 1, lambda = 10^seq(3, -3, length = 100))
print(csa)

#save the graphy
pdf("pics/TieMa_homework1_Q5_b_lasso.pdf", height = 9, width = 12)
plot(lasso.mod, xvar = "lambda", label = TRUE, main = "lasso regression")
dev.off()


##################################################################################################################################
#Q5-C-1 Estimate the parameters by OLS using the full sample.

# create a table
parameters_of_Q5<- matrix(rep(NA),9,1)

coef1 <- coef(lm(y_THE_LOGWAGE ~ x, data= data_question5))
print(summary(lm(y_THE_LOGWAGE ~ x, data= data_question5)))

#fill it with data
parameters_of_Q5[,1] <- coef1

#put the row name 
rownames(parameters_of_Q5) <- colnames(data_question5)
colnames(parameters_of_Q5)[1] <-'OLS'

#summon the table!
print(parameters_of_Q5)


##################################################################################################################################
#Q5-C-2 Which variables are statistically significant at the 10% level?

#yes, its just same things..
#I realize I was over thigns again....

something <- lm(y_THE_LOGWAGE ~ x, data= data_question5)
something_summary <- summary(something)
print(something_summary)


#the school2_scale are statistically signifcant at 10% level (squared school 1 plus scale)


##################################################################################################################################
#Q5-D Based on the BIC and BIC,  which variables are included in the best model?


regfit.all <- regsubsets(y_THE_LOGWAGE~. , data_question5, nvmax = 10)
#using the function regsubsets to find the best subset for teh funtion, 
#cp is AIC

something_summary_1<- summary(regfit.all)

par(mfrow=c(1,2))
plot(something_summary_1$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(something_summary_1$cp)
plot(regfit.all, scale = "bic")
layout(1)

pdf("pics/TieMa_homework1_Q5_d.pdf", height = 9, width = 12)
par(mfrow=c(1,2))
plot(something_summary_1$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(something_summary_1$cp)
plot(regfit.all, scale = "bic")

layout(1)
dev.off()

##################################################################################################################################
#Q5 -E Estimate the lasso coefficients using the full sample. Are any of the coefficients forced
#to be exactly 0? Compare your results with OLS and best subset selection.

evil_grid <- 10^seq(3, -3, length = 100)
lasso.mod <- glmnet(x, y_THE_LOGWAGE, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE)

lasso.cv <- cv.glmnet(x, y_THE_LOGWAGE, alpha = 1, nfolds = 5)
print(lasso.cv)

model3 <- glmnet(x, y_THE_LOGWAGE, alpha = 1, lambda = lasso.cv$lambda.min)
coef3 <- coef(model3)
print (coef3)

###########################################################################################################################
#Q5 - f 


# split the sample into train/test sets
train <- sample(nrow(data_question5), round(nrow(data_question5)/2))
cv.error <- rep(NA,3)

# least squares
ols.cv <- lm(y_THE_LOGWAGE ~ x, data = data_question5, subset = train)
  #set the 
cv.error[1] <- mean((y_THE_LOGWAGE - predict(ols.cv,  data_question5))[-train]^2)

# ridge
ridge.cv <- cv.glmnet(x[train,], y_THE_LOGWAGE[train], alpha = 0, nfolds = 10)
ridge.lam <- ridge.cv$lambda.min

cv.error[2] <- mean((y_THE_LOGWAGE - predict(ridge.cv, s = ridge.lam, newx = x))[-train]^2)

# lasso
lasso.cv <- cv.glmnet(x[train,], y_THE_LOGWAGE[train], alpha = 1, nfolds = 10)
lasso.lam <- lasso.cv$lambda.min
cv.error[3] <- mean((y_THE_LOGWAGE - predict(lasso.cv, s = lasso.lam, newx = x))[-train]^2)

cv.error <- data.frame(cv.error)
rownames(cv.error) <- c("ols","ridge","lasso")
cv.error

plot(ridge.cv)


#what is the different between the function predicrt



###########################################################################################################################
#This is the longest,the most challange homework I ever down... 
#now I can finally rest in peace



x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000
res <- data.frame()
system.time({
  trial <- 1
  while(trial <= trials) {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    r <- coefficients(result1)
    res <- rbind(res, r)
    trial <- trial + 1
  }
})

detectCores(logical = F)  # 8
mc <- getOption("mc.cores", 10)
system.time({
  res <- mclapply(1:5000000, fun, mc.cores = mc);
})
stopCluster(mc)
