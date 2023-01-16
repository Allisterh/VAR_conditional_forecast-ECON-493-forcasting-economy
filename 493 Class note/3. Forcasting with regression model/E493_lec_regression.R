## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, cache = FALSE, warning = FALSE, message = FALSE, 
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = '99%'
)

# packages
library(pacman)
p_load(tidyverse, lmtest, fpp2, boot, leaps)

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



## ---- echo=TRUE------------------------------------------------------------------
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


## ---- echo=TRUE------------------------------------------------------------------
# estimate models
model1 <- lm(y ~ x1)	# missing variables
model2 <- lm(y ~ x1 + x2 + x3)	# correctly specified model
model3 <- lm(y ~ x1 + x2 + x3 + x4 + x5)	# irrelevant variables


## ---- echo=TRUE------------------------------------------------------------------
# estimate models
coeftest(model2)	# correctly specified model


## ---- echo=TRUE------------------------------------------------------------------
# estimate models
coeftest(model3)	# irrelevant variables


## ---- echo=TRUE------------------------------------------------------------------
# add new observation
new <- list(x1 = 1, x2 = 1, x3 = 1, x4 = 1, x5 = 1)

# predict y using model 2
pred_new <- predict(
    model2, 
    newdata = new, 
    se.fit = TRUE, 
    interval = "prediction"
  )
pred_new$fit


## ---- echo=TRUE------------------------------------------------------------------
CV(model1)
CV(model2)
CV(model3)


## ---- echo=TRUE------------------------------------------------------------------
# validation set approach
cv.error <- rep(NA,3)
# split the sample into train/test sets
train <- sample(nrow(data), round(nrow(data)/2))
# model 1
model1.cv <- lm(y ~ x1, subset = train)
cv.error[1] <- mean((y - predict(model1.cv, data))[-train]^2)
# model 2
model2.cv <- lm(y ~ x1 + x2 + x3, subset = train)
cv.error[2] <- mean((y - predict(model2.cv, data))[-train]^2)
# model 3
model3.cv <- lm(y ~ x1 + x2 + x3 + x4 + x5, subset = train)
cv.error[3] <- mean((y - predict(model3.cv, data))[-train]^2)
# results
print(cv.error)


## ---- echo=TRUE------------------------------------------------------------------
# leave-one-out cross-validation
cv.error.1 <- rep(NA,3)
# model 1
model1.cv <- glm(y ~ x1)
cv.error.1[1] <- cv.glm(data, model1.cv)$delta[1]
# model 2
model2.cv <- glm(y ~ x1 + x2 + x3)
cv.error.1[2] <- cv.glm(data, model2.cv)$delta[1]
# model 3
model3.cv <- glm(y ~ x1 + x2 + x3 + x4 + x5)
cv.error.1[3] <- cv.glm(data, model3.cv)$delta[1]
# results
print(cv.error.1)


## ---- echo=TRUE------------------------------------------------------------------
# k-fold cross-validation
cv.error.2 <- rep(NA,3)
# model 1
model1.cv <- glm(y ~ x1)
cv.error.2[1] <- cv.glm(data, model1.cv, K = 10)$delta[1]
# model 2
model2.cv <- glm(y ~ x1 + x2 + x3)
cv.error.2[2] <- cv.glm(data, model2.cv, K = 10)$delta[1]
# model 3
model3.cv <- glm(y ~ x1 + x2 + x3 + x4 + x5)
cv.error.2[3] <- cv.glm(data, model3.cv, K = 10)$delta[1]
# results
print(cv.error.2)


## ---- echo=TRUE------------------------------------------------------------------
CV(model1)
CV(model2)
CV(model3)


## ---- echo=TRUE------------------------------------------------------------------
regfit.all <- regsubsets(y~. , data = data, nvmax = 5)
(reg.summary <- summary(regfit.all))


## ---- echo=TRUE------------------------------------------------------------------
# ssr statistics
print(reg.summary$rss)

# bic statistics
print(reg.summary$bic)

# cp statistics (same model as aic)
print(reg.summary$cp)

# adjusted-rsq statistics
print(reg.summary$adjr2)


## ---- echo=FALSE, fig.height=2.8-------------------------------------------------
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


## ---- echo=TRUE------------------------------------------------------------------
# plot variable
plot(regfit.all, scale = "bic")


## ---- echo=TRUE------------------------------------------------------------------
# plot variable
plot(regfit.all, scale = "Cp")


## ---- echo=TRUE------------------------------------------------------------------
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


## ---- echo=TRUE------------------------------------------------------------------
# data summary
data %>%
  dplyr::select(price, age, odometer) %>%
  summary()


## ---- echo=F---------------------------------------------------------------------
#
ggplot(data, aes(x = age, y = price)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), color = "darkorange") +
  coord_cartesian(xlim = c(0,30), ylim = c(0,20000))


## ---- echo=F---------------------------------------------------------------------
# 
ggplot(data, aes(x = odometer, y = price)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), color = "darkorange") +
  coord_cartesian(xlim = c(0,30), ylim = c(0,20000))


## ---- echo=F---------------------------------------------------------------------
# estimate models
model1 <- as.formula(price ~ age + agesq)
model2 <- as.formula(price ~ age + agesq + odometer + odometersq)
model3 <- as.formula(price ~ age + agesq + odometer + odometersq + 
                       LE + XLE + SE + 
                       cond_likenew + cond_excellent + cond_good)
model4 <- as.formula(price ~ age + agesq + odometer + odometersq + 
                       LE + XLE + SE + 
                       cond_likenew + cond_excellent + cond_good + cylind6)
# 
reg1 <- lm(model1, data=data)
reg2 <- lm(model2, data=data)
reg3 <- lm(model3, data=data)
reg4 <- lm(model4, data=data)


## ---- echo=TRUE------------------------------------------------------------------
# estimate models
coeftest(reg2)	


## ---- echo=TRUE------------------------------------------------------------------
# add new observation
new <- 
  list(
    age = 10, agesq = 10^2, odometer = 12, odometersq = 12^2,
    SE = 0, XLE = 0, LE = 1, 
    cond_likenew = 0, cond_excellent = 1, cond_good = 0, cylind6 = 0, 
    price=NA
  )
# predict price with model 3
pred_new <- predict(reg3, newdata = new, se.fit = TRUE, interval = "prediction")
pred_new$fit


## ---- echo=TRUE------------------------------------------------------------------
# evaluation of the models
table <- rbind(CV(reg1), CV(reg2), CV(reg3), CV(reg4))
rownames(table) <- c('Model1', 'Model2', 'Model3', 'Model4')
table


## ---- echo=TRUE------------------------------------------------------------------
# select variables
data2 <- data %>% 
  dplyr::select(
    age, agesq, odometer, odometersq, SE, LE, XLE, 
    cond_likenew, cond_excellent, cond_good, price, 
    cylind6
  )
# 
regfit.all.2 <- regsubsets(price~. , data = data2, nvmax = 10)
reg.summary.2 <- summary(regfit.all.2)


## ---- echo=FALSE, fig.height=2.8-------------------------------------------------
# plot results
par(mfrow=c(1,3))
plot(reg.summary.2$rss, xlab = "Number of Variables", ylab = "SSR", type = "l")
plot(reg.summary.2$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
m.bic <- which.min(reg.summary.2$bic)
points(m.bic, reg.summary.2$bic[m.bic], col = my_colors[3], cex = 2, pch = 20)
plot(reg.summary.2$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(reg.summary.2$cp)
points(m.cp, reg.summary.2$cp[m.cp], col = my_colors[3], cex = 2, pch = 20)
layout(1)


## ---- echo=TRUE------------------------------------------------------------------
# plot variable
plot(regfit.all.2, scale = "bic")


## ---- echo=F---------------------------------------------------------------------
#
ggplot(data, aes(x = age, y = lnprice)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), color = "darkorange") +
  coord_cartesian(xlim = c(0,30), ylim = c(5,10))


## ---- echo=F---------------------------------------------------------------------
#
ggplot(data, aes(x = odometer, y = lnprice)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), color = "darkorange") +
  coord_cartesian(xlim = c(0,30), ylim = c(5,10))


## ---- echo=TRUE------------------------------------------------------------------
# select variables
data3 <- data %>% 
  dplyr::select(
    age, agesq, odometer, odometersq, SE, LE, XLE, 
    cond_likenew, cond_excellent, cond_good, lnprice, 
    cylind6
  )
# 
regfit.all.3 <- regsubsets(lnprice~. , data = data3, nvmax = 10)
reg.summary.3 <- summary(regfit.all.3)


## ---- echo=FALSE, fig.height=2.8-------------------------------------------------
# plot results
par(mfrow=c(1,3))
plot(reg.summary.3$rss, xlab = "Number of Variables", ylab = "SSR", type = "l")
plot(reg.summary.3$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
m.bic <- which.min(reg.summary.3$bic)
points(m.bic, reg.summary.3$bic[m.bic], col = my_colors[3], cex = 2, pch = 20)
plot(reg.summary.3$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
m.cp <- which.min(reg.summary.3$cp)
points(m.cp, reg.summary.3$cp[m.cp], col = my_colors[3], cex = 2, pch = 20)
layout(1)


## ---- echo=TRUE------------------------------------------------------------------
# plot variable
plot(regfit.all.3, scale = "bic")

