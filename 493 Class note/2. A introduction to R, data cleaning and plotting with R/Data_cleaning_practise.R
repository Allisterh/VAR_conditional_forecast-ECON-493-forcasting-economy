## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo=TRUE, cache=FALSE, warning=FALSE, message=FALSE, 
  fig.align="center",
  fig.width=7,
  fig.height=3.5,
  out.width='99%'
)

# loding the package packages
library(fontawesome)
library(pacman)
p_load(tidyverse, lmtest, maps, geosphere)

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

# set seed
set.seed(1234)



## ---- indeeddotcom, echo = F, fig.height = 4, out.width='96%'------------------
# The popularity data
pop_df =
  data.frame(
    lang = c("SQL", "Python", "R", "SAS", "Matlab", "SPSS", "Stata"),
    n_jobs = c(107130, 66976, 48772, 25644, 11464, 3717, 1624),
    free = c(T, T, T, F, F, F, F)
  )
## Plot it
pop_df %>%
  mutate(lang = lang %>% factor(ordered = T)) %>%
  ggplot(aes(x = lang, y = n_jobs, fill = free)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  aes(x = reorder(lang, -n_jobs), fill = reorder(free, -free)) +
  xlab("Statistical language") +
  scale_y_continuous(label = scales::comma) +
  ylab("Number of jobs") +
  labs(
    title = "Comparing statistical languages",
    subtitle = "Number of job postings on Indeed.com, 2019/01/06"
  ) +
  scale_fill_manual(
    "Free?",
    labels = c("True", "False"),
    values = c(my_colors[2], my_colors[1])
  ) +
  ggthemes::theme_pander(base_size = 14) +
  theme(legend.position = "bottom")


## ---- eval=FALSE---------------------------------------------------------------
## install.packages("tidyverse")


## ------------------------------------------------------------------------------
library(tidyverse)
library(lmtest)


## ---- cache=FALSE--------------------------------------------------------------
version$version.string


## ----eval=FALSE----------------------------------------------------------------
## RStudio.Version()$version
## # requires an interactive session but should return
## # something like "[1] ‘2022.7.2.576’"


## ----Coding 1, eval=FALSE------------------------------------------------------
## > (11-2
## +


## ----Calc 1--------------------------------------------------------------------
123 + 456 + 789


## ----Calc 2--------------------------------------------------------------------
sqrt(400)


## ------------------------------------------------------------------------------
?sqrt


## ----Objects 1-----------------------------------------------------------------
new.object <- 144


## ----Objects 2-----------------------------------------------------------------
new.object


## ----Objects 3-----------------------------------------------------------------
new.object + 10
new.object + new.object
sqrt(new.object)


## ----Vectors 1-----------------------------------------------------------------
new.object <- c(4, 9, 16, 25, 36)
new.object


## ----Vectors 2-----------------------------------------------------------------
sqrt(new.object)


## ------------------------------------------------------------------------------
string.vector <- c("Atlantic", "Pacific", "Arctic")
string.vector


## ------------------------------------------------------------------------------
factor.vector <- factor(string.vector)
factor.vector


## ------------------------------------------------------------------------------
new.object <- 1:10 # Making vector of 1 to 10 


## ---- eval=FALSE---------------------------------------------------------------
## save(new.object, file = "new_object.RData")


## ---- eval=FALSE---------------------------------------------------------------
## load("new_object.RData")


## ---- eval=FALSE---------------------------------------------------------------
## getwd()


## ---- eval=FALSE---------------------------------------------------------------
## setwd()


## ------------------------------------------------------------------------------
head(cars, 4) # prints first 4 rows, see tail() too


## ------------------------------------------------------------------------------
str(cars) 


## ------------------------------------------------------------------------------
cars2 <- as_tibble(cars)
head(cars2,4)
str(cars2)


## ------------------------------------------------------------------------------
summary(cars)


## ------------------------------------------------------------------------------
# histogram
hist(cars$dist) 


## ------------------------------------------------------------------------------
# histogram
hist(
  cars$dist, xlab = "Distance (ft)", 
  main = "Observed stopping distances of cars" 
) 


## ------------------------------------------------------------------------------
( dist_mean  <- mean(cars$dist) )
( speed_mean <- mean(cars$speed) )


## ----fit-----------------------------------------------------------------------
# linear regression
fit <- lm(dist ~ speed, data = cars)
summary(fit)


## ------------------------------------------------------------------------------
# use coeftest function from lmtest package for cleaner output
coeftest(fit)


## ------------------------------------------------------------------------------
# see the structure of lm() output using str()
str(fit)


## ----mtcars_baseplot-----------------------------------------------------------
# another ugly plot
plot(cars$speed, cars$dist)
abline(fit, col = "blue")


## ----mtcars_ggplot-------------------------------------------------------------
# a nicer plot with ggplot2
ggplot(data = cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm") 


## ---- eval=FALSE---------------------------------------------------------------
## new_df <- read.csv("some_spreadsheet.csv")


## ---- echo=TRUE----------------------------------------------------------------
# read data
data <- read.csv("data/midterm1.csv", header = FALSE)
# set up data frame
grades <- 
  data.frame(
    student = data$V1,
    hw1 = data$V2,
    hw2 = data$V3,
    mid1 = data$V4
  )
# compute midterm mean
mean(grades$mid1, na.rm = TRUE)


## ---- echo=TRUE----------------------------------------------------------------
# histogram of midterm grades
ggplot(grades, aes(x = mid1)) +
  geom_dotplot(bins = 12) + xlab("midterm 1") + ylab("frequency")


## ---- echo=TRUE----------------------------------------------------------------
# midterm grades and homework 1
ggplot(grades, aes(x = hw1, y = mid1)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~x, color = "darkorange") + 
  coord_cartesian(xlim = c(0,50), ylim = c(0,60))


## ---- echo=TRUE----------------------------------------------------------------
# midterm grades and homework 2
ggplot(grades, aes(x = hw2, y = mid1)) +
  geom_point(color = "grey40", alpha = 0.8) + 
  geom_smooth(method = 'lm', formula = y~x, color = "darkorange") + 
  coord_cartesian(xlim = c(0,50), ylim = c(0,60))


## ---- echo=TRUE----------------------------------------------------------------
# regression
mod1 <- lm(mid1 ~ hw1 + hw2, data = grades)
coeftest(mod1)


## ---- echo=FALSE---------------------------------------------------------------
par(mar=c(0,0,0,0))
map(
  'world',
  col = "#f2f2f2",
  fill = TRUE,
  bg = "white",
  lwd = 0.05,
  mar = rep(0,4), 
  border = 0, 
  ylim = c(-80,80),
  xlim = c(-200,160)
)


## ---- echo=FALSE---------------------------------------------------------------
par(mar=c(0,0,0,0))
map(
  'world',
  col = "#f2f2f2",
  fill = TRUE,
  bg = "white",
  lwd = 0.05,
  mar = rep(0,4), 
  border = 0,
  ylim = c(-80,80), 
  xlim = c(-200,160)
)
# 
mvd <- c(-56,-35)
sea <- c(-122,47)
yeg <- c(-113,53)
data <- data.frame(rbind(mvd,sea,yeg))
colnames(data) <- c("long","lat")

points(x = data$long, y = data$lat, col = "steelblue2", cex = 1, pch = 20)
text(mvd[1]+10, mvd[2], "MVD", cex=.8)
text(sea[1]-10, sea[2], "SEA", cex=.8)
text(yeg[1]+10, yeg[2], "YEG", cex=.8)


## ---- echo=FALSE---------------------------------------------------------------
par(mar=c(0,0,0,0))
map(
  'world',
  col = "#f2f2f2", 
  fill = TRUE,
  bg = "white",
  lwd = 0.05,
  mar = rep(0,4),
  border = 0,
  ylim = c(-80,80),
  xlim = c(-200,160)
)

mvd <- c(-56,-35)
sea <- c(-122,47)
yeg <- c(-113,53)
data <- data.frame(rbind(mvd,sea,yeg))
colnames(data) <- c("long","lat")

points(x = data$long, y = data$lat, col = "steelblue2", cex = 1, pch = 20)
text(mvd[1]+10, mvd[2], "MVD", cex=.8)
text(sea[1]-10, sea[2], "SEA", cex=.8)
text(yeg[1]+10, yeg[2], "YEG", cex=.8)

# connection between Montevideo and Seattle
inter <- gcIntermediate(mvd,  sea, n=50, addStartEnd=TRUE, breakAtDateLine=F) 
lines(inter, col="steelblue2", lwd=2)


## ---- echo=FALSE---------------------------------------------------------------
par(mar=c(0,0,0,0))
map(
  'world',
  col = "#f2f2f2",
  fill = TRUE,
  bg = "white",
  lwd = 0.05,
  mar = rep(0,4),
  border = 0,
  ylim = c(-80,80),
  xlim = c(-200,160)
)

mvd <- c(-56,-35)
sea <- c(-122,47)
yeg <- c(-113,53)
data <- data.frame(rbind(mvd,sea,yeg))
colnames(data) <- c("long","lat")

points(x = data$long, y = data$lat, col = "steelblue2", cex = 1, pch = 20)
text(mvd[1]+10, mvd[2], "MVD", cex=.8)
text(sea[1]-10, sea[2], "SEA", cex=.8)
text(yeg[1]+10, yeg[2], "YEG", cex=.8)

# connection between Montevideo and Seattle
inter <- gcIntermediate(mvd,  sea, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col="steelblue2", lwd=2)

# between Seattle and Edmonton
inter <- gcIntermediate(sea,  yeg, n=50, addStartEnd=TRUE, breakAtDateLine=F)
lines(inter, col="steelblue2", lwd=2)

