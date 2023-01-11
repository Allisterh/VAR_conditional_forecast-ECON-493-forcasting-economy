install.packages("tidyverse")
install.packages("lmtest")
library("tidyverse")
library("ggplot2")

#the tip
  #i wantto have the same bersion of hte software as my student
  #bUT I DO'T WANT AN UPDATE TO break anything
  # do not update anything until you are down.

# squrt(400)

#An object's name represet tthe information store in that object, so you can treat the obect's name as it it were the vaule stored inside

#An vestoryt is seris of elements such as numbers, you can cereate a vector and store it as an obejct in the same way. 
#the function can do the al the timne.


# we ofthen work with. dat that are cartegorical, to created a vector of rtest - strings in programming term
  #we must place the txt in quotes
  #string vecotr

#categrica data can aso bes stored as a factor, which has a underlying nunmberic
  #presentation, model witll convert factors to dummies
  #factor? 

#the same priples can be used tp. create more complex obejct like matrices, arrys lists and daterframe
#most data sets you will work with will be road into R and stored as dataframe
# so this course will mainly focus on ,mainpulating and visulaizing these objets. 

#The the same priciple  can be used to create more complsx tobjects lie matrix, arrys list anda datafreamas
#most data sets you will work with will be read into and stored as data frame so this cource will ianly foucisng

#so, other wat of rreathing obejctive, you have a lot of tip for creatinga lot of thing, 

# you need to save it. 

#save(The class note, file = "wulala gei")
# this thing need to using the save the enviroment it can help on saivng both the script. 

data(cars)
View(cars)

#strs 
  #Can tell you which kind of data you got.
# you can create a data
# when you looking at the table. 
cars2 <- as_tibble(cars)
head(cars2)
summary(cars)

# The histgrame
# how to access only one bvairlce

str(cars$speed)
# if you put an asisgnment suchas x<-y in parenteses (), R will print the outut of hte assignmnet out for tou in our document.

#Linear regress ion 
fit <- lm(dist ~ speed, data = cars)
summary(fit)

str(fit)

#another ugly ploy
plot(cars$speed, cars$dist)
abline()

#a nicer ploy with ggplots
ggplot(data = cars, aes(x = speed, y = dist)) + geom_point() + geom_smooth(method = "lm")

#working with acutally data
  #using the function of read.cvs

#hwo to rea it
  #package foreign
    #part of base R
  #package haven
    # paryt of the tidyverse family

#the data set miderml.cvs
#th header = false/ture 
  #the first linke of the first code is eqmpty or not.

#na_rm = ture
  #remove the missing the value form the varble. 

#coord_cartestian ()  
  #set the grade size that you need to do. 


#clanfear.github.io/Csss508
ddd 
# R programming 
  
