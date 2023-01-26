## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(pacman)
p_load(tidyverse, lmtest, fpp2, glmnet, leaps)

# 
options(digits = 4)

# set transparent background 
theme_set(theme_grey())
theme_update(
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

# set the seed
set.seed(1234)



## ---- echo=TRUE---------------------------------------------------------
# simulate data
n.obs <- 200
n.var <- 3
# 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors
x <- matrix(0, n.obs, n.var)
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) }
# 
y <- .75*x[,1] + .50*x[,2] + e
x <- scale(x)
data.all <- data.frame(x,y)


## ---- echo=TRUE---------------------------------------------------------
model1 <- lm(y ~ X1 + X2 + X3, data = data.all)
coeftest(model1)


## ---- echo=TRUE---------------------------------------------------------
# ridge reg for different values of lambda
grid <- 10^seq(3, -3, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)


## ---- echo=TRUE---------------------------------------------------------
# plot ridge results
plot(ridge.mod, xvar = "lambda", label = TRUE)


## ---- echo=TRUE---------------------------------------------------------
# ridge
ridge.mod$lambda[1]
coef(ridge.mod)[,1]
# least squares
model1$coef


## ---- echo=TRUE---------------------------------------------------------
# ridge
ridge.mod$lambda[80]
coef(ridge.mod)[,80]
# least squares
model1$coef


## ---- echo=TRUE---------------------------------------------------------
# lasso reg for different values of lambda
grid <- 10^seq(3, -3, length = 100)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)


## ---- echo=TRUE, fig.height=3.5-----------------------------------------
# plot ridge results
plot(lasso.mod, xvar = "lambda", label = TRUE)


## ---- echo=TRUE---------------------------------------------------------
# ridge
lasso.mod$lambda[1]
coef(lasso.mod)[,1]
# least squares
model1$coef


## ---- echo=TRUE---------------------------------------------------------
# ridge
lasso.mod$lambda[80]
coef(lasso.mod)[,80]
# least squares
model1$coef


## ---- echo=TRUE---------------------------------------------------------
# simulate data
n.obs <- 200
n.var <- 50
# 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors
x <- matrix(0, n.obs, n.var)
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) }
# 
y <- .75*x[,1] + .50*x[,2] + e
x <- scale(x)
data.all <- data.frame(x,y)


## ---- echo=TRUE---------------------------------------------------------
# 
grid <- 10^seq(3, -3, length = 100)

# ridge reg for different values of lambda
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

# lasso reg for different values of lambda
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)


## ---- echo=TRUE---------------------------------------------------------
# plot ridge results
plot(ridge.mod, xvar = "lambda", label = TRUE)


## ---- echo=TRUE---------------------------------------------------------
# plot lasso results
plot(lasso.mod, xvar = "lambda", label = TRUE)


## ---- echo=TRUE---------------------------------------------------------
# split the sample into train/test sets
train <- sample(nrow(data.all), round(nrow(data.all)/2))
cv.error <- rep(NA,3)

# least squares
ols.cv <- lm(y ~ x, data = data.all, subset = train)
cv.error[1] <- mean((y - predict(ols.cv, data.all))[-train]^2)

# ridge
ridge.cv <- cv.glmnet(x[train,], y[train], alpha = 0, nfolds = 10)
ridge.lam <- ridge.cv$lambda.min
cv.error[2] <- mean((y - predict(ridge.cv, s = ridge.lam, newx = x))[-train]^2)

# lasso
lasso.cv <- cv.glmnet(x[train,], y[train], alpha = 1, nfolds = 10)
lasso.lam <- lasso.cv$lambda.min
cv.error[3] <- mean((y - predict(lasso.cv, s = lasso.lam, newx = x))[-train]^2)


## ---- echo=TRUE---------------------------------------------------------
# plot ridge cv
plot(ridge.cv)


## ---- echo=TRUE---------------------------------------------------------
# plot lasso cv
plot(lasso.cv)


## ---- echo=TRUE---------------------------------------------------------
cv.error <- data.frame(cv.error)
rownames(cv.error) <- c("ols","ridge","lasso")
cv.error


## ---- echo=TRUE---------------------------------------------------------
# least squares coefficients
model1 <- lm(y ~ x, data = data.all)
coef.ls <- data.frame("x" = seq(1:n.var), "beta" = coef(model1)[-1])

# ridge coefficients
model2 <- glmnet(x, y, alpha = 0, lambda = ridge.lam)
coef.ridge <- data.frame("x" = seq(1:n.var), "beta" = coef(model2)[-1])

# lasso coefficients
model3 <- glmnet(x, y, alpha = 1, lambda = lasso.lam)
coef.lasso <- data.frame("x" = seq(1:n.var), "beta" = coef(model3)[-1])


## ---- echo=F------------------------------------------------------------
# least squares coefficients
ggplot(coef.ls) + geom_col(aes(x = x, y = beta), fill = "steelblue") + 
  ggtitle("Estimated coefficients: OLS") +
  xlab("variable") + ylab("coefficients") + 
  coord_cartesian(ylim = c(-1,1))


## ---- echo=F------------------------------------------------------------
# ridge coefficients
ggplot(coef.ridge) + geom_col(aes(x = x, y = beta), fill = "steelblue") + 
  ggtitle("Estimated coefficients: ridge") +
  xlab("variable") + ylab("coefficients") + 
  coord_cartesian(ylim = c(-1,1))


## ---- echo=F------------------------------------------------------------
# lasso coefficients
ggplot(coef.lasso) + geom_col(aes(x = x, y = beta), fill = "steelblue") + 
  ggtitle("Estimated coefficients: lasso") +
  xlab("variable") + ylab("coefficients") + 
  coord_cartesian(ylim = c(-1,1))


## ---- echo=TRUE---------------------------------------------------------
# estimate the lasso selected model
coeftest(lm(y ~ X1 + X2 + X11 + X41, data = data.all))


## ---- echo=TRUE---------------------------------------------------------
# read data
data <- 
  read.csv(
    "data/used_cars_work.csv", 
    header = TRUE, 
    stringsAsFactors = TRUE
  )
# focus only on Chicago
data <- data %>%
  filter(area == "chicago")


## ---- echo=TRUE---------------------------------------------------------
# select variables
x <- data %>% 
  dplyr::select(
    age, agesq, odometer, odometersq, SE, LE, XLE, 
    cond_likenew, cond_excellent, cond_good, cylind6
  ) %>% 
  data.matrix()
# 
price <- data$price
data2 <- data.frame(x,price)
# 
regfit.all <- regsubsets(price~. , data = data2, nvmax = 10)
reg.summary <- summary(regfit.all)


## ---- echo=FALSE, fig.height=2.8----------------------------------------
# plot results
par(mfrow=c(1,3))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "SSR", type = "l")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
m.bic <- which.min(reg.summary$bic)
points(m.bic, reg.summary$bic[m.bic], col = my_colors[3], cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(reg.summary$cp)
points(m.cp, reg.summary$cp[m.cp], col = my_colors[3], cex = 2, pch = 20)
layout(1)


## ---- echo=TRUE---------------------------------------------------------
# plot variable
plot(regfit.all, scale = "bic")


## ---- echo=TRUE---------------------------------------------------------
# lasso reg for different values of lambda
lasso.mod <- glmnet(x, price, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE)


## ---- echo=F------------------------------------------------------------
# least squares coefficients
coef1 <- coef(lm(price ~ x))

# best subset selection coefficients
coef2 <- coef(regfit.all, id = which.min(reg.summary$bic))

# lasso regression coefficients
lasso.cv <- cv.glmnet(x, price, alpha = 1, nfolds = 5)
model3 <- glmnet(x, price, alpha = 1, lambda = lasso.cv$lambda.min)
coef3 <- coef(model3)


## ---- echo=FALSE--------------------------------------------------------
# table
table1 <- matrix(rep(NA),12,3)
table1[,1] <- coef1
table1[c(1:6),2] <- coef2
table1[,3] <- coef3[,1]
table1 <- data.frame(table1)
colnames(table1) <- c("ols", "bss", "lasso")
rownames(table1) <- rownames(coef3)
table1


## ---- echo=TRUE---------------------------------------------------------
# select variables
x <- data %>% 
  dplyr::select(
    age, agesq, odometer, odometersq, SE, LE, XLE, 
    cond_likenew, cond_excellent, cond_good, cylind6
  ) %>% 
  data.matrix()
# 
lnprice <- data$lnprice
data2 <- data.frame(x,lnprice)
# 
regfit.all <- regsubsets(lnprice~. , data = data2, nvmax = 10)
reg.summary <- summary(regfit.all)


## ---- echo=FALSE, fig.height=2.8----------------------------------------
# plot results
par(mfrow=c(1,3))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "SSR", type = "l")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
m.bic <- which.min(reg.summary$bic)
points(m.bic, reg.summary$bic[m.bic], col = my_colors[3], cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(reg.summary$cp)
points(m.cp, reg.summary$cp[m.cp], col = my_colors[3], cex = 2, pch = 20)
layout(1)


## ---- echo=TRUE---------------------------------------------------------
# plot variable
plot(regfit.all, scale = "bic")


## ---- echo=TRUE---------------------------------------------------------
# lasso reg for different values of lambda
lasso.mod <- glmnet(x, lnprice, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE)


## ---- echo=F------------------------------------------------------------
# least squares coefficients
coef1 <- coef(lm(lnprice ~ x))

# best subset selection coefficients
coef2 <- coef(regfit.all, id = which.min(reg.summary$bic))

# lasso regression coefficients
lasso.cv <- cv.glmnet(x, lnprice, alpha = 1, nfolds = 5)
model3 <- glmnet(x, lnprice, alpha = 1, lambda = lasso.cv$lambda.min)
coef3 <- coef(model3)


## ---- echo=FALSE--------------------------------------------------------
# table
table1 <- matrix(rep(NA),12,3)
table1[,1] <- coef1
table1[c(1,2,4,6),2] <- coef2
table1[,3] <- c[oef3,1]
table1 <- data.frame(table1)
colnames(table1) <- c("ols", "bss", "lasso")
rownames(table1) <- rownames(coef3)
table1


## ---- echo=TRUE---------------------------------------------------------
# simulate data
n.obs <- 200
n.var <- 500
# 
e <- rnorm(n.obs, mean = 0, sd = 2) # errors
x <- matrix(0, n.obs, n.var)
for (i in 1:n.var){ x[,i] <- rnorm(n.obs) }
# 
y <- .75*x[,1] + .50*x[,2] + e
x <- scale(x)
data.all <- data.frame(x,y)


## ---- echo=TRUE---------------------------------------------------------
# lasso reg for different values of lambda
lasso.mod <- glmnet(x, y, alpha = 1, lambda = 10^seq(3, -3, length = 100))
plot(lasso.mod, xvar = "lambda", label = TRUE)


## ---- echo=TRUE---------------------------------------------------------
lasso.cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
plot(lasso.cv)


## ---- echo=F------------------------------------------------------------
# lasso coefficients
model4 <- glmnet(x, y, alpha = 1, lambda = lasso.cv$lambda.min)
coef.lasso <- data.frame("x" = seq(1:n.var), "beta" = coef(model4)[-1])
ggplot(coef.lasso) + geom_col(aes(x = x, y = beta), fill = "steelblue") + 
  ggtitle("Estimated coefficients: lasso") +
  xlab("variable") + ylab("coefficients") + 
  coord_cartesian(ylim = c(-1,1))


## ---- echo=TRUE---------------------------------------------------------
# 
coef.ols <- rep(NA, n.var)
test.ols <- rep(NA, n.var)
for (i in 1:n.var){
  tmp0 <- lm(y ~ x[,i])
  coef.ols[i] <- coeftest(tmp0)[2,1] # estimated slope
  test.ols[i] <- abs(coeftest(tmp0)[2,3]) # t-stat
  }
# single regression coefficients
coef.ols <- 
  data.frame(
    "x" = seq(1:n.var), 
    "beta" = coef.ols, 
    "test" = test.ols
  )


## ---- echo=FALSE--------------------------------------------------------
# single regression coefficients
ggplot(coef.ols) + geom_col(aes(x = x, y = beta), fill = "steelblue") + 
  ggtitle("Estimated coefficients: OLS") +
  xlab("variable") + ylab("coefficients") + 
  coord_cartesian(ylim = c(-1,1))


## ---- echo=FALSE--------------------------------------------------------
# single regression t-tests
ggplot(coef.ols) + 
  geom_col(aes(x = x, y = test), fill = "steelblue") + 
  geom_hline(yintercept = 1.96, linetype = "dashed", color = my_colors[3]) + 
  ggtitle("t-statistics: OLS") +
  xlab("variable") + ylab("t-statistic") + 
  coord_cartesian(ylim = c(0,7))

