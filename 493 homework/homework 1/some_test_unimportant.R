
library("ggplot2")
library("fpp2")
library("glmnet")
library("mosaicCalc")#微积分计算器！
library("tidyr")
library("lmtest")
library("boot")


set.seed(1234)
n.obs <- 100
x1 <- rnorm(n.obs)
x2 <- x1^2
x3 <- x1^3
x4 <- x1^4
y <- x1 - 2*x1^2 + rnorm(n.obs)
data_set<- data.frame(y, x1, x2, x3, x4)

model_one <- lm(y ~ x1)
summary(model_one)
coef(model_one)
vcov(model_one)
fitted(model_one)
coeftest(model_one)


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
evil_grid <- 10^seq(3, -3, length = 100)
ridge.mod <- glmnet(x, y_THE_LOGWAGE, alpha = 0, lambda = evil_grid)

#plot ridge results
plot(ridge.mod, xvar = "lambda", label = TRUE, main = "ridge regression standardized coeﬃcients")

#save the graphy
pdf("pics/TieMa_homework1_Q5_b_ridge.pdf", height = 9, width = 12)
plot(ridge.mod, xvar = "lambda", label = TRUE, main = "ridge regression")
dev.off()

#plot the lasso
evil_grid <- 10^seq(3, -3, length = 100)
lasso.mod <- glmnet(x, y_THE_LOGWAGE, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE, main = "lasso regression")

#save the graphy
pdf("pics/TieMa_homework1_Q5_b_lasso.pdf", height = 9, width = 12)
plot(lasso.mod, xvar = "lambda", label = TRUE, main = "lasso regression")
dev.off()


##################################################################################################################################
#Q5-C-1 Estimate the parameters by OLS using the full sample.

# create a table
parameters_of_Q5<- matrix(rep(NA),9,1)

coef1 <- coef(lm(y_THE_LOGWAGE ~ x, data= data_question5))

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


regfit.all <- regsubsets(y_THE_LOGWAGE~. , data_question5, nvmax = 8, method = "backward")
print(regfit.all)
something_summary_1<- summary(regfit.all)

something_summary$


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



###########################################################################################################################
#This is the longest,the most challange homework I ever down... 
#now I can finally rest in peace


# simulate data 
n.obs <- 200 
n.var <- 3 # 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors 
x <- matrix(0, n.obs, n.var) 
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) } # 
y <- .75*x[,1] + .50*x[,2] + e 
x <- scale(x) 
data.all <- data.frame(x,y)

model1 <- lm(y ~ X1+ X2 +X3, data = data.all)
coeftest(model1)
summary(model1)

grid <- 10^seq(3, -3, length = 100) 
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
summary(ridge.mod)
plot(ridge.mod, xvar = "lambda", label = TRUE)

coef(ridge.mod)
cv.glmnet(model1)






# Load the forecast library
library(forecast)

# Generate example data
x <- abs(rnorm(10000, mean = 0, sd= 1)

# Find the optimal value of lambda
lambda <- BoxCox.lambda(x)
print(lambda)
# Apply the Box-Cox transformation
x_transformed <- BoxCox(x, lambda)
plot(x_transformed)





