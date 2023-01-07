# the evil chapter 5, finall back to the code itself
# the linear model.
  # the simple linear regression, you can see it from the BLUE, best linear regression omodel

install.packages("fpp2")
install.packages("GGally")
library(fpp2)
library(GGally)

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#> `geom_smooth()` using formula 'y ~ x'

tslm(Consumption ~ Income, data=uschange) #强制拟合
#> 
#> Call:
#> tslm(formula = Consumption ~ Income, data = uschange)
#> 
#> Coefficients:
#> (Intercept)       Income  
#>       0.545        0.281
#>       

# the multiple linear regression 

install.packages("fpp2")

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

#the assumption
  #they have mean zero; otherwise the forecasts will be systematically biased.
    #they are not autocorrelated; otherwise the forecasts will be inefficient, 
#as there is more information in the data that can be exploited.
  #they are unrelated to the predictor variables; otherwise there would
    #be more information that should be included in the systematic part of the model.


#5.2 the least squares estimation
  
